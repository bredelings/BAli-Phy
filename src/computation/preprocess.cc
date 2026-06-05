#include <iostream>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include "computation/preprocess.H"
#include "computation/machine/graph_register.H"
#include "computation/module.H"
#include "operations.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/indexify.H"
#include "computation/expression/constructor.H"
#include "computation/expression/expression.H" // for is_reglike( )
#include "computation/expression/convert.H" // for to_core_exp( )
#include "computation/expression/runtime_views.H"
#include "computation/runtime/ast.H"
#include "computation/runtime/trim.H"
#include "computation/fresh_vars.H"
#include "haskell/ids.H"
#include "util/variant.H"

using std::optional;
using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;

namespace
{

Core::Var<> get_named_core_var(int n)
{
    if (n < 26)
        return Core::Var<>(string{char('a' + n)});
    else
        return Core::Var<>("v" + std::to_string(n - 26));
}

Core::Var<> direct_reg_core_var(int r)
{
    return Core::Var<>("<" + std::to_string(r) + ">");
}

Core::Var<> env_reg_core_var(int r)
{
    return Core::Var<>("[" + std::to_string(r) + "]");
}

Core::Exp<> runtime_only_core_exp(const string& text)
{
    return Core::RuntimeOnly{text};
}

Core::Constant runtime_constant_to_core(const Runtime::Exp& E)
{
    Core::Constant C;

    if (auto x = E.to<Runtime::Int>())
        C.value = x->value;
    else if (auto x = E.to<Runtime::Double>())
        C.value = double(x->value);
    else if (auto x = E.to<Runtime::Char>())
        C.value = x->value;
    else if (auto x = E.to<Runtime::String>())
        C.value = x->value;
    else if (auto x = E.to<Runtime::Integer>())
        C.value = integer_container(x->value);
    else
        throw std::runtime_error("Runtime expression is not a Core constant");

    return C;
}

}

namespace Runtime
{
    Exp untranslate_vars(const Exp& E, const map<int, string>& ids)
    {
        return E.visit([&](const auto& e) -> Exp
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, RegRef>)
            {
                auto loc = ids.find(e.target);
                if (loc != ids.end())
                    return GlobalVar(var(loc->second));
                else
                    return E;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return Lambda(untranslate_vars(e.body, ids));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<Exp> binds;
                binds.reserve(e.binds.size());
                for(const auto& bind: e.binds)
                    binds.push_back(untranslate_vars(bind, ids));

                return Let(std::move(binds), untranslate_vars(e.body, ids));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;
                alts.reserve(e.alts.size());
                for(const auto& alt: e.alts)
                    alts.push_back(Alt(alt.pattern, untranslate_vars(alt.body, ids)));

                return Case(untranslate_vars(e.object, ids), std::move(alts));
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<Exp> args;
                args.reserve(e.args.size());
                for(const auto& arg: e.args)
                    args.push_back(untranslate_vars(arg, ids));

                return App(e.head, std::move(args));
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                return Trim(e.indices, untranslate_vars(e.body, ids));
            }
            else
                return E;
        });
    }

    Exp untranslate_vars(const Exp& E, const map<string, int>& ids)
    {
        map<int, string> reg_to_name;
        for(const auto& [name, reg]: ids)
            reg_to_name[reg] = name;

        return untranslate_vars(E, reg_to_name);
    }
}

namespace
{

Core::Pattern<> runtime_deindexify_pattern(const Runtime::Pattern& pattern, vector<Core::Var<>>& variables)
{
    return std::visit([&](const auto& p) -> Core::Pattern<>
    {
        using T = std::decay_t<decltype(p)>;

        if constexpr (std::is_same_v<T, Runtime::WildcardPattern>)
            return {};
        else if constexpr (std::is_same_v<T, Runtime::ConstructorPattern>)
        {
            Core::Pattern<> pattern2;
            pattern2.head = p.head.name();

            for(int i = 0; i < p.head.n_args(); i++)
            {
                auto x = get_named_core_var(variables.size());
                pattern2.args.push_back(x);
                variables.push_back(x);
            }

            return pattern2;
        }
        else
            std::abort();
    }, pattern);
}

Core::Exp<> runtime_deindexify(const Runtime::Exp& E, vector<Core::Var<>>& variables)
{
    if (E.to<Runtime::Int>() or E.to<Runtime::Double>() or E.to<Runtime::Char>() or
        E.to<Runtime::String>() or E.to<Runtime::Integer>())
    {
        return runtime_constant_to_core(E);
    }
    else if (E.to<Runtime::LogDouble>())
        return runtime_only_core_exp(E.print());
    else if (auto e = E.to<Runtime::Constructor>())
    {
        if (e->value.n_args() != 0)
            return runtime_only_core_exp(E.print());

        return Core::ConApp<>{e->value.name(), {}};
    }
    else if (auto e = E.to<Runtime::ObjectValue>())
        return runtime_only_core_exp(e->value ? e->value->print() : string{"null"});
    else if (auto e = E.to<Runtime::IndexVar>())
    {
        if (e->index >= variables.size())
            return Core::Var<>("[?" + std::to_string(e->index) + "]");

        return Core::Exp<>(variables[variables.size() - 1 - e->index]);
    }
    else if (auto e = E.to<Runtime::GlobalVar>())
    {
        return Core::Var<>(e->name.name, e->name.index, {}, e->name.is_exported);
    }
    else if (auto e = E.to<Runtime::RegRef>())
        return direct_reg_core_var(e->target);
    else if (auto e = E.to<Runtime::Lambda>())
    {
        auto x = get_named_core_var(variables.size());
        variables.push_back(x);
        auto body = runtime_deindexify(e->body, variables);
        variables.pop_back();

        return Core::Lambda<>{x, body};
    }
    else if (auto e = E.to<Runtime::Let>())
    {
        Core::Decls<> decls;
        decls.reserve(e->binds.size());

        for(int i = 0; i < e->binds.size(); i++)
        {
            auto x = get_named_core_var(variables.size());
            variables.push_back(x);
            decls.push_back({x, {}});
        }

        for(int i = 0; i < e->binds.size(); i++)
            decls[i].body = runtime_deindexify(e->binds[i], variables);

        auto body = runtime_deindexify(e->body, variables);

        for(int i = 0; i < e->binds.size(); i++)
            variables.pop_back();

        return Core::Let<>{decls, body};
    }
    else if (auto e = E.to<Runtime::Case>())
    {
        auto object = runtime_deindexify(e->object, variables);
        vector<Core::Alt<>> alts;
        alts.reserve(e->alts.size());

        for(const auto& alt: e->alts)
        {
            auto old_size = variables.size();
            auto pattern = runtime_deindexify_pattern(alt.pattern, variables);
            auto body = runtime_deindexify(alt.body, variables);
            variables.resize(old_size);
            alts.push_back({pattern, body});
        }

        return Core::Case<>{object, std::move(alts)};
    }
    else if (auto e = E.to<Runtime::App>())
    {
        vector<Core::Exp<>> args;
        args.reserve(e->args.size());
        for(const auto& arg: e->args)
            args.push_back(runtime_deindexify(arg, variables));

        if (std::holds_alternative<Runtime::FunctionApply>(e->head))
        {
            if (args.size() < 2)
            {
                Core::Exp<> result = runtime_only_core_exp("apply");
                for(const auto& arg: args)
                    result = Core::Apply<>{result, arg};
                return result;
            }

            Core::Exp<> result = args[0];
            for(int i = 1; i < args.size(); i++)
                result = Core::Apply<>{result, args[i]};
            return result;
        }
        else if (auto head = std::get_if<Runtime::ConstructorApp>(&e->head))
        {
            return Core::ConApp<>{head->head.name(), args};
        }
        else
        {
            auto op_head = std::get_if<Runtime::OperationApp>(&e->head);
            if (not op_head or not op_head->head or op_head->lib_name.empty() or op_head->func_name.empty() or op_head->call_conv.empty())
            {
                Core::Exp<> result = runtime_only_core_exp(op_head and op_head->head ? op_head->head->print() : string{"op"});
                for(const auto& arg: args)
                    result = Core::Apply<>{result, arg};
                return result;
            }

            void* op = nullptr;
            if (op_head->call_conv == "ecall")
                op = reinterpret_cast<void*>(op_head->head->e_op);
            else
                op = reinterpret_cast<void*>(op_head->head->op);

            if (not op)
            {
                Core::Exp<> result = runtime_only_core_exp(op_head->head->print());
                for(const auto& arg: args)
                    result = Core::Apply<>{result, arg};
                return result;
            }

            return Core::BuiltinOp<>{op_head->lib_name, op_head->func_name, op_head->call_conv, args, op};
        }
    }
    else if (E.to<Runtime::Trim>())
    {
        return runtime_deindexify(Runtime::trim_unnormalize(E), variables);
    }
    else
        std::abort();
}

}

Core::Exp<> runtime_deindexify(const Runtime::Exp& E, const vector<Core::Var<>>& variables)
{
    auto variables2 = variables;
    return runtime_deindexify(E, variables2);
}

Core::Exp<> runtime_deindexify(const Runtime::Exp& E)
{
    return runtime_deindexify(E, vector<Core::Var<>>{});
}

Core::Exp<> runtime_deindexify(const closure& C)
{
    vector<Core::Var<>> variables;
    variables.reserve(C.Env.size());
    for(int r: C.Env)
        variables.push_back(env_reg_core_var(r));

    return runtime_deindexify(trim_unnormalize(C).get_code(), variables);
}

Core::Exp<> runtime_unprepare_for_translation(const Runtime::Exp& E, const map<int, string>& ids)
{
    auto E2 = Runtime::untranslate_vars(E, ids);
    E2 = Runtime::trim_unnormalize(E2);
    return runtime_deindexify(E2);
}

Core::Exp<> runtime_unprepare_for_translation(const Runtime::Exp& E, const map<string, int>& ids)
{
    map<int, string> reg_to_name;
    for(const auto& [name, reg]: ids)
        reg_to_name[reg] = name;

    return runtime_unprepare_for_translation(E, reg_to_name);
}


Core::Decls<> graph_normalize(FreshVarSource& source, Core::Decls<> decls);
Core::Exp<> graph_normalize(FreshVarSource& source, const Core::Exp<>& E);

Core::Exp<> make_let(const Core::Decls<>& decls, const Core::Exp<>& body)
{
    if (decls.empty())
        return body;
    else
        return Core::Let<>{decls, body};
}

bool is_simple_core_arg(const Core::Exp<>& E, bool sub_exp_ok)
{
    if (E.to_var())
        return true;

    if (sub_exp_ok)
    {
        if (E.to_constant())
            return true;

        if (auto B = E.to_builtinOp(); B and B->call_conv == "ecall")
            return true;
    }

    return false;
}

std::tuple<Core::Decls<>, Core::Exp<>>
graph_normalize_lift(FreshVarSource& source, const Core::Exp<>& E, bool sub_exp_ok)
{
    Core::Decls<> decls;

    if (sub_exp_ok)
    {
        if (auto B = E.to_builtinOp(); B and B->call_conv == "ecall")
        {
            auto E2 = *B;

            for(auto& arg: E2.args)
            {
                auto [decls2, arg2] = graph_normalize_lift(source, arg, true);
                arg = arg2;
                std::ranges::move(decls2, std::back_inserter(decls));
            }

            return {decls, E2};
        }
    }

    auto E2 = graph_normalize(source, E);

    if (not is_simple_core_arg(E2, sub_exp_ok))
    {
        auto x = source.get_fresh_core_var("gn");
        decls.push_back({x, E2});
        E2 = x;
    }

    return {decls, E2};
}

Core::Decls<> graph_normalize(FreshVarSource& source, Core::Decls<> decls)
{
    for(auto& [_, e]: decls)
        e = graph_normalize(source, e);

    return decls;
}

Core::Exp<> graph_normalize(FreshVarSource& source, const Core::Exp<>& E)
{
    if (E.empty())
        return E;
    else if (E.to_var() or E.to_constant() or E.to_runtimeOnly())
        return E;
    else if (auto L = E.to_lambda())
        return Core::Lambda<>{L->x, graph_normalize(source, L->body)};
    else if (auto A = E.to_apply())
    {
        auto [head_decls, head] = graph_normalize_lift(source, A->head, false);
        auto [arg_decls, arg] = graph_normalize_lift(source, A->arg, false);

        head_decls.insert(head_decls.end(), arg_decls.begin(), arg_decls.end());
        return make_let(head_decls, Core::Apply<>{head, arg});
    }
    else if (auto L = E.to_let())
    {
        auto decls = graph_normalize(source, L->decls);
        auto body = graph_normalize(source, L->body);
        return Core::Let<>{decls, body};
    }
    else if (auto C = E.to_case())
    {
        auto [decls, object] = graph_normalize_lift(source, C->object, true);
        auto alts = C->alts;
        for(auto& [_, body]: alts)
            body = graph_normalize(source, body);

        return make_let(decls, Core::Case<>{object, alts});
    }
    else if (auto C = E.to_conApp())
    {
        Core::Decls<> decls;
        auto args = C->args;
        for(auto& arg: args)
        {
            auto [decls2, arg2] = graph_normalize_lift(source, arg, false);
            arg = arg2;
            std::ranges::move(decls2, std::back_inserter(decls));
        }

        return make_let(decls, Core::ConApp<>{C->head, args});
    }
    else if (auto B = E.to_builtinOp())
    {
        Core::Decls<> decls;
        auto args = B->args;
        bool sub_exp_ok = B->call_conv == "ecall";
        for(auto& arg: args)
        {
            auto [decls2, arg2] = graph_normalize_lift(source, arg, sub_exp_ok);
            arg = arg2;
            std::ranges::move(decls2, std::back_inserter(decls));
        }

        return make_let(decls, Core::BuiltinOp<>{B->lib_name, B->func_name, B->call_conv, args, B->op});
    }
    else
        std::abort();
}

// Deleted: Fun_normalize( ).  Perhaps reintroduce later.
// Translate from language Basic to language Fun by introducing letrec expressions.  (arguments are already vars)
// See "From Natural Semantics to C: A Formal Derivation of two STG machines."
//      by Alberto de la Encina and Ricardo Pena.

Runtime::Exp runtime_prepare_for_translation(FreshVarSource& source, const Core::Exp<>& E)
{
    auto E2 = graph_normalize(source, E);
    auto R = runtime_indexify(E2);
    Runtime::check_invariants(R);
    R = Runtime::trim_normalize(R);
    Runtime::check_invariants(R);
    return R;
}

Runtime::Exp runtime_prepare_for_translation(FreshVarState& state, const Core::Exp<>& E)
{
    FreshVarSource source(state);
    return runtime_prepare_for_translation(source, E);
}

closure translate_prepared(reg_heap& heap, Runtime::Exp E, closure&& C)
{
    Runtime::check_invariants(E);
    E = heap.translate_refs(E, C.Env);
    Runtime::check_translated(E);
    C.set_code( std::move(E) );
    return std::move(C);
}

closure translate_and_trim(reg_heap& heap, Runtime::Exp E, closure&& C)
{
    Runtime::check_invariants(E);
    E = heap.capture_local_reg_refs(E, C.Env);
    Runtime::check_invariants(E);
    E = Runtime::trim_normalize(E);
    return translate_prepared(heap, E, std::move(C));
}

closure reg_heap::preprocess(Runtime::Exp E, closure::Env_t Env)
{
    closure C;
    C.Env = std::move(Env);
    return translate_and_trim(*this, E, std::move(C));
}

closure reg_heap::preprocess_prepared(Runtime::Exp E, closure::Env_t Env)
{
    closure C;
    C.Env = std::move(Env);
    return translate_prepared(*this, E, std::move(C));
}

closure reg_heap::preprocess(const Core::Exp<>& E)
{
    return preprocess_prepared(runtime_prepare_for_translation(fresh_var_state, E));
}

int reg_heap::reg_for_id(const var& x)
{
    auto& name = x.name;
    assert(is_qualified_symbol(name) or is_haskell_builtin_con_name(name));
    auto loc = identifiers.find( x.name );
    if (loc == identifiers.end())
    {
	if (is_haskell_builtin_con_name(name))
	{
            assert(x.index == 0);

            auto sym = lookup_builtin_symbol(name);
            auto code = to_core_exp(to<CoreUnfolding>(sym->unfolding)->expr);
            add_identifier(x.name);

	    // get the root for each identifier
	    loc = identifiers.find(x.name);
	    assert(loc != identifiers.end());

	    int R = loc->second;

	    assert(R != -1);
	    set_C(R, preprocess(code) );
	}
	else
	    throw myexception()<<"Can't translate undefined identifier '"<<x<<"' in expression!";
    }

    return loc->second;
}
