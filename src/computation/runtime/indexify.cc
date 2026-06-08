#include "computation/runtime/indexify.H"
#include "computation/closure.H"
#include "computation/operation.H"
#include "util/variant.H"
#include "computation/haskell/Integer.H" // for Integer
#include "computation/haskell/ids.H"
#include "computation/runtime/ast.H"
#include "computation/runtime/trim.H"

#include <stdexcept>

using std::vector;
using std::string;

template <typename T>
std::optional<int> find_index_backward(const vector<T>& v,const T& t)
{
    int L = v.size();
    for(int i=0;i<L;i++)
	if (v[L-i-1] == t)
	    return i;
    return {};
}

bool is_global_var(const Core::Var<>& x)
{
    return is_qualified_symbol(x.name) or is_haskell_builtin_con_name(x.name);
}

Runtime::Exp atom_from_constant(const Core::Constant& C)
{
    if (auto c = to<char>(C.value))
        return Runtime::Char(*c);
    else if (auto i = to<int>(C.value))
        return Runtime::Int(*i);
    else if (auto i = to<integer_container>(C.value))
        return Runtime::Integer(i->i);
    else if (auto d = to<double>(C.value))
        return Runtime::Double(*d);
    else if (auto s = to<std::string>(C.value))
        return Runtime::String(*s);
    else
        std::abort();
}

Runtime::Exp indexify(const Core::Exp<>& E, vector<Core::Var<>>& variables)
{
    // Variable
    if (auto V = E.to_var())
    {
        auto index = find_index_backward(variables, *V);
        if (not index)
        {
            if (is_global_var(*V))
                return Runtime::GlobalVar(V->name);
            else
                throw myexception()<<"Variable '"<<E<<"' is apparently not a bound variable in '"<<E<<"'?";
        }
        else
            return Runtime::IndexVar(*index);
    }
    // Lambda expression - /\x.e
    else if (auto L = E.to_lambda())
    {
        variables.push_back(L->x);

        auto E2 = Runtime::Lambda(indexify(L->body, variables));

        variables.pop_back();

        return E2;
    }
    // Apply expression
    else if (auto A = E.to_apply())
    {
        auto head = indexify(A->head, variables);
        auto arg = indexify(A->arg, variables);

        return Runtime::FunctionApp({head, arg});
    }
    // Let expression
    else if (auto L = E.to_let())
    {
        for(auto& [x,_]: L->decls)
            variables.push_back(x);

        vector<Runtime::Exp> binds;
        for(auto& [_,e]: L->decls)
            binds.push_back(indexify(e, variables));

        auto body = indexify(L->body, variables);

        for(int i=0;i<L->decls.size();i++)
            variables.pop_back();

        return Runtime::Let(binds, body);
    }

    // case expression
    else if (auto C = E.to_case())
    {
        auto object2 = indexify(C->object, variables);

        vector<Runtime::Alt> alts2;
        for(auto& [pattern, body]: C->alts)
        {
            Runtime::Pattern pattern2;
            Runtime::Exp body2;

            if (pattern.is_wildcard_pat())
            {
                pattern2 = Runtime::WildcardPattern{};
                body2 = indexify(body, variables);
            }
            else
            {
                pattern2 = Runtime::ConstructorPattern(*pattern.head, pattern.args.size());

                for(auto& arg: pattern.args)
                    variables.push_back(arg);

                body2 = indexify(body, variables);

                for(auto& _: pattern.args)
                    variables.pop_back();
            }

            alts2.push_back(Runtime::Alt(pattern2, body2));
        }

        return Runtime::Case(object2, alts2);
    }
    else if (auto C = E.to_conApp())
    {
        vector<Runtime::Exp> args;
        for(auto& arg: C->args)
            args.push_back(indexify(arg, variables));

        return Runtime::ConstructorApp(C->head, C->args.size(), args);
    }
    else if (auto B = E.to_builtinOp())
    {
        vector<Runtime::Exp> args;
        for(auto& arg: B->args)
            args.push_back(indexify(arg, variables));

        return Runtime::OperationApp(Runtime::builtin_operation_app(B->op, B->lib_name, B->func_name, B->call_conv), args);
    }
    else if (auto C = E.to_constant())
        return atom_from_constant(*C);
    else if (E.to_runtimeOnly())
        throw myexception()<<"indexify: expression '"<<E<<"' is not representable as Runtime code.";

    std::abort();
}

Runtime::Exp indexify(const Core::Exp<>& E)
{
    vector<Core::Var<>> variables;
    return indexify(E, variables);
}

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

Core::Pattern<> deindexify_pattern(const Runtime::Pattern& pattern, vector<Core::Var<>>& variables)
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

Core::Exp<> deindexify(const Runtime::Exp& E, vector<Core::Var<>>& variables)
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
        return Core::Var<>(e->name, e->index);
    }
    else if (auto e = E.to<Runtime::RegRef>())
        return direct_reg_core_var(e->target);
    else if (auto e = E.to<Runtime::Lambda>())
    {
        auto x = get_named_core_var(variables.size());
        variables.push_back(x);
        auto body = deindexify(e->body, variables);
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
            decls[i].body = deindexify(e->binds[i], variables);

        auto body = deindexify(e->body, variables);

        for(int i = 0; i < e->binds.size(); i++)
            variables.pop_back();

        return Core::Let<>{decls, body};
    }
    else if (auto e = E.to<Runtime::Case>())
    {
        auto object = deindexify(e->object, variables);
        vector<Core::Alt<>> alts;
        alts.reserve(e->alts.size());

        for(const auto& alt: e->alts)
        {
            auto old_size = variables.size();
            auto pattern = deindexify_pattern(alt.pattern, variables);
            auto body = deindexify(alt.body, variables);
            variables.resize(old_size);
            alts.push_back({pattern, body});
        }

        return Core::Case<>{object, std::move(alts)};
    }
    else if (auto e = E.to<Runtime::FunctionApp>())
    {
        vector<Core::Exp<>> args;
        args.reserve(e->args.size());
        for(const auto& arg: e->args)
            args.push_back(deindexify(arg, variables));

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
    else if (auto e = E.to<Runtime::ConstructorApp>())
    {
        vector<Core::Exp<>> args;
        args.reserve(e->args.size());
        for(const auto& arg: e->args)
            args.push_back(deindexify(arg, variables));

        return Core::ConApp<>{e->head.name(), args};
    }
    else if (auto e = E.to<Runtime::OperationApp>())
    {
        vector<Core::Exp<>> args;
        args.reserve(e->args.size());
        for(const auto& arg: e->args)
            args.push_back(deindexify(arg, variables));

        if (not e->head or e->lib_name.empty() or e->func_name.empty() or e->call_conv.empty())
        {
            Core::Exp<> result = runtime_only_core_exp(e->head ? e->head->print() : string{"op"});
            for(const auto& arg: args)
                result = Core::Apply<>{result, arg};
            return result;
        }

        void* op = nullptr;
        if (e->call_conv == "ecall")
            op = reinterpret_cast<void*>(e->head->e_op);
        else
            op = reinterpret_cast<void*>(e->head->op);

        if (not op)
        {
            Core::Exp<> result = runtime_only_core_exp(e->head->print());
            for(const auto& arg: args)
                result = Core::Apply<>{result, arg};
            return result;
        }

        return Core::BuiltinOp<>{e->lib_name, e->func_name, e->call_conv, args, op};
    }
    else if (E.to<Runtime::Trim>())
    {
        return deindexify(Runtime::trim_unnormalize(E), variables);
    }
    else
        std::abort();
}

}

Core::Exp<> deindexify(const Runtime::Exp& E, const vector<Core::Var<>>& variables)
{
    auto variables2 = variables;
    return deindexify(E, variables2);
}

Core::Exp<> deindexify(const Runtime::Exp& E)
{
    return deindexify(E, vector<Core::Var<>>{});
}

Core::Exp<> deindexify(const closure& C)
{
    vector<Core::Var<>> variables;
    variables.reserve(C.Env.size());
    for(int r: C.Env)
        variables.push_back(env_reg_core_var(r));

    return deindexify(trim_unnormalize(C).get_code(), variables);
}
