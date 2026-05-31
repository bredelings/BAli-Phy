#include <iostream>
#include <memory>
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
    else if (E.to_var() or E.to_constant())
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
    C.set_runtime_expression(std::move(E));
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
