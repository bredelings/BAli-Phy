#include "indexify.H"
#include "util/variant.H"
#include "computation/haskell/Integer.H" // for Integer
#include "computation/haskell/ids.H"
#include "computation/runtime/ast.H"

using std::vector;
using std::string;

template <typename T>
int find_index_backward(const vector<T>& v,const T& t)
{
    int L = v.size();
    for(int i=0;i<L;i++)
	if (v[L-i-1] == t)
	    return i;
    return -1;
}

bool is_global_var(const var& x)
{
    return is_qualified_symbol(x.name) or is_haskell_builtin_con_name(x.name);
}

template <typename NoteV>
var to_runtime_var(const Core::Var<NoteV>& x)
{
    return var(x.name, x.index, x.is_exported);
}

Runtime::Exp runtime_atom_from_constant(const Core::Constant& C)
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

Runtime::Exp runtime_indexify(const Core::Exp<>& E, vector<Core::Var<>>& variables)
{
    // Variable
    if (auto V = E.to_var())
    {
        int index = find_index_backward(variables, *V);
        if (index == -1)
        {
            auto x = to_runtime_var(*V);
            if (is_global_var(x))
                return Runtime::GlobalVar(x);
            else
                throw myexception()<<"Variable '"<<E<<"' is apparently not a bound variable in '"<<E<<"'?";
        }
        else
            return Runtime::IndexVar(index);
    }
    // Lambda expression - /\x.e
    else if (auto L = E.to_lambda())
    {
        variables.push_back(L->x);

        auto E2 = Runtime::Lambda(runtime_indexify(L->body, variables));

        variables.pop_back();

        return E2;
    }
    // Apply expression
    else if (auto A = E.to_apply())
    {
        auto head = runtime_indexify(A->head, variables);
        auto arg = runtime_indexify(A->arg, variables);

        return Runtime::App(Runtime::FunctionApply{}, {head, arg});
    }
    // Let expression
    else if (auto L = E.to_let())
    {
        for(auto& [x,_]: L->decls)
            variables.push_back(x);

        vector<Runtime::Exp> binds;
        for(auto& [_,e]: L->decls)
            binds.push_back(runtime_indexify(e, variables));

        auto body = runtime_indexify(L->body, variables);

        for(int i=0;i<L->decls.size();i++)
            variables.pop_back();

        return Runtime::Let(binds, body);
    }

    // case expression
    else if (auto C = E.to_case())
    {
        auto object2 = runtime_indexify(C->object, variables);

        vector<Runtime::Alt> alts2;
        for(auto& [pattern, body]: C->alts)
        {
            Runtime::Pattern pattern2;
            Runtime::Exp body2;

            if (pattern.is_wildcard_pat())
            {
                pattern2 = Runtime::WildcardPattern{};
                body2 = runtime_indexify(body, variables);
            }
            else
            {
                pattern2 = Runtime::ConstructorPattern(*pattern.head, pattern.args.size());

                for(auto& arg: pattern.args)
                    variables.push_back(arg);

                body2 = runtime_indexify(body, variables);

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
            args.push_back(runtime_indexify(arg, variables));

        return Runtime::App(Runtime::ConstructorApp(C->head, C->args.size()), args);
    }
    else if (auto B = E.to_builtinOp())
    {
        vector<Runtime::Exp> args;
        for(auto& arg: B->args)
            args.push_back(runtime_indexify(arg, variables));

        return Runtime::App(Runtime::builtin_operation_app(B->op, B->lib_name, B->func_name, B->call_conv), args);
    }
    else if (auto C = E.to_constant())
        return runtime_atom_from_constant(*C);
    else if (E.to_runtimeOnly())
        throw myexception()<<"runtime_indexify: expression '"<<E<<"' is not representable as Runtime code.";

    std::abort();
}

Runtime::Exp runtime_indexify(const Core::Exp<>& E)
{
    vector<Core::Var<>> variables;
    return runtime_indexify(E, variables);
}
