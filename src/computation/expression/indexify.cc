#include "indexify.H"
#include "constructor.H"
#include "var.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "index_var.H"
#include "apply.H"
#include "util/variant.H"
#include "computation/haskell/Integer.H" // for Integer
#include "computation/haskell/ids.H"
#include "computation/expression/reg_var.H"
#include "computation/runtime/ast.H"

using std::pair;
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

var get_named_var(int n)
{
    if (n<26) 
	return var(string{char(97+n)});
    else
	return var(n-26);
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
                pattern2 = Runtime::ConstructorPattern(constructor(*pattern.head, pattern.args.size()));

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

        auto c = constructor(C->head, C->args.size());
        return Runtime::App(Runtime::ConstructorApp(c), args);
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

    std::abort();
}

Runtime::Exp runtime_indexify(const Core::Exp<>& E)
{
    vector<Core::Var<>> variables;
    return runtime_indexify(E, variables);
}

/// Convert to using de Bruijn indices.
expression_ref deindexify(const expression_ref& E, const vector<expression_ref>& variables)
{
    // Lambda expression - /\x.e
    if (E.head().is_a<lambda2>())
    {
	vector<expression_ref> variables2 = variables;
	var d = get_named_var(variables.size());
	variables2.push_back(d);
	return lambda_quantify(d,deindexify(E.sub()[0],variables2));
    }

    // Let expression
    else if (auto L = parse_indexed_let_expression(E))
    {
	vector<expression_ref> variables2 = variables;

        vector<pair<var,expression_ref>> decls;
	for(auto& bind: L->binds)
	{
	    var d = get_named_var(variables2.size());
	    decls.push_back({d, bind});
	    variables2.push_back( d );
	}

	// Deindexify let-bound stmts only after list of variables has been extended.
	for(auto& [x,rhs]: decls)
	    rhs = deindexify(rhs, variables2);

	auto body = deindexify(L->body, variables2);

	return let_expression(decls, body);
    }

    // case expression
    else if (auto C = parse_case_expression(E))
    {
        auto& [object, alts] = *C;

        object = deindexify(object, variables);

	for(auto& [pattern, body]: alts)
	{
	    assert(not pattern.size());
	    // Make a new expression so we can add variables to the pattern if its a constructor

	    // Find the number of arguments in the constructor
	    int n_args = 0;
	    if (pattern.head().is_a<constructor>())
		n_args = pattern.head().as_<constructor>().n_args();

	    // Add n_arg variables to the stack and to the pattern
	    vector<expression_ref> variables2 = variables;
	    for(int j=0;j<n_args;j++)
	    {
		var d = get_named_var(variables2.size());
		variables2.push_back( d );
		pattern = pattern + d;
	    }

#ifndef NDEBUG
	    if(is_var(pattern))
		assert(is_wildcard(pattern));
#endif

	    body = deindexify(body, variables2);
	}

	return make_case_expression(object, alts);
    }

    // Indexed Variable - This is assumed to be a free variable, so just shift it.
    else if (E.is_index_var())
    {
        int index = E.as_index_var();
        if (index >= variables.size())
            return index_var(index - variables.size());

        return variables[variables.size()-1 - index];
    }
    // Pinned runtime register references are already literal references.
    else if (E.is_reg_var())
        return E;
    // Constant or 0-arg constructor
    else if (is_literal_type(E.type()) or is_constructor(E))
        return E;
    else if (is_constructor_exp(E) or is_apply_exp(E) or E.head().is_a<Operation>())
    {
        // This handles (modifiable) with no arguments.
        if (E.is_atomic())
            return E;
        else
        {
            object_ptr<expression> V = E.as_expression().clone();
            for(int i=0;i<V->size();i++)
                V->sub[i] = deindexify(V->sub[i], variables);
            return V;
        }
    }

    std::cerr<<"failing to deindexify expression "<<E<<"\n";
    std::abort();
}

expression_ref deindexify(const expression_ref& E)
{
    return deindexify(E,vector<expression_ref>{});
}
