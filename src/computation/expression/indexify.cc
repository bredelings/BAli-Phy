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

expression_ref make_indexed_lambda(const expression_ref& R)
{
    return new expression(lambda2(),{R});
}

var get_named_var(int n)
{
    if (n<26) 
	return var(string{char(97+n)});
    else
	return var(n-26);
}

/// Convert to using de Bruijn indices.
expression_ref indexify(const expression_ref& E, vector<var>& variables)
{
    // Lambda expression - /\x.e
    if (E.head().is_a<lambda>())
    {
	variables.push_back(E.sub()[0].as_<var>());
	auto E2 = make_indexed_lambda( indexify(E.sub()[1], variables) );
	variables.pop_back();
	return E2;
    }

    // Let expression
    else if (is_let_expression(E))
    {
        auto& L = E.as_<let_exp>();
	for(auto& [x,_]: L.binds)
	    variables.push_back(x);

        vector<expression_ref> es;
	for(auto& [_,e]: L.binds)
	    es.push_back( indexify(e, variables) );

        auto body = indexify(L.body, variables);

	for(int i=0;i<L.binds.size();i++)
	    variables.pop_back();

	return indexed_let_expression(es,body);
    }

    // case expression
    else if (auto case_exp = parse_case_expression(E))
    {
        auto& [object, alts] = *case_exp;

	object = indexify(object, variables);

	for(auto& [pattern, body]: alts)
	{
	    // Handle C x[1..n] -> body[i]

#ifndef NDEBUG
	    // FIXME - I guess this doesn't handle case a of b -> f(b)?
	    if (is_var(pattern))
		assert(is_wildcard(pattern));
#endif

	    for(int j=0;j<pattern.size();j++)
		variables.push_back(pattern.sub()[j].as_<var>());

	    body = indexify(body, variables);

	    for(int j=0;j<pattern.size();j++)
		variables.pop_back();

	    pattern = pattern.head();
	}

	return make_case_expression(object, alts);
    }

    // Indexed Variable - This is assumed to be a free variable, so just shift it.
    else if (E.is_index_var())
        return index_var(E.as_index_var() + variables.size());

    // Variable
    else if (E.is_a<var>())
    {
        auto& D = E.as_<var>();
        assert(not is_wildcard(E));

        int index = find_index_backward(variables, D);
        if (index == -1)
            throw myexception()<<"Dummy '"<<D<<"' is apparently not a bound variable in '"<<E<<"'?";
        else
            return index_var(index);
    }
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
                V->sub[i] = indexify(V->sub[i], variables);
            return V;
        }
    }

    std::abort();
}

expression_ref indexify(const expression_ref& E)
{
    vector<var> variables;
    return indexify(E,variables);
}

/// Convert to using de Bruijn indices.
expression_ref indexify(const Core2::Exp<>& E, vector<Core2::Var<>>& variables)
{
    // Variable
    if (auto V = E.to_var())
    {
        int index = find_index_backward(variables, *V);
        if (index == -1)
            throw myexception()<<"Variable '"<<E<<"' is apparently not a bound variable in '"<<E<<"'?";
        else
            return index_var(index);
    }
    // Lambda expression - /\x.e
    else if (auto L = E.to_lambda())
    {
	variables.push_back(L->x);

	auto E2 = make_indexed_lambda( indexify(L->body, variables) );

	variables.pop_back();

	return E2;
    }
    // Apply expression
    else if (auto A = E.to_apply())
    {
	auto head = indexify(A->head, variables);
        auto arg = indexify(A->arg, variables);

	return apply_expression(head, arg);
    }
    // Let expression
    else if (auto L = E.to_let())
    {
	for(auto& [x,_]: L->decls)
	    variables.push_back(x);

        vector<expression_ref> es;
	for(auto& [_,e]: L->decls)
	    es.push_back( indexify(e, variables) );

        auto body = indexify(L->body, variables);

	for(int i=0;i<L->decls.size();i++)
	    variables.pop_back();

	return indexed_let_expression(es, body);
    }

    // case expression
    else if (auto C = E.to_case())
    {
	auto object2 = indexify(C->object, variables);

	Core::Alts alts2;
	for(auto& [pattern, body]: C->alts)
	{
	    // Handle C x[1..n] -> body[i]
	    expression_ref pattern2;
	    expression_ref body2;
	    if (pattern.is_wildcard_pat())
	    {
		pattern2 = var(-1);
		body2 = indexify(body,variables);
	    }
	    else
	    {
		pattern2 = constructor(*pattern.head, pattern.args.size());

		for(auto& arg: pattern.args)
                    variables.push_back(arg);

		body2 = indexify(body, variables);

		for(auto& _: pattern.args)
                    variables.pop_back();
	    }
	    alts2.push_back({pattern2, body2});
	}

	return make_case_expression(object2, alts2);
    }
    else if (auto C = E.to_conApp())
    {
	vector<expression_ref> args;
	for(auto& arg: C->args)
	    args.push_back( indexify(arg, variables) );

	auto c = constructor(C->head, C->args.size());
	return expression_ref{c,args};
    }
    else if (auto B = E.to_builtinOp())
    {
	vector<expression_ref> args;
	for(auto& arg: B->args)
	    args.push_back( indexify(arg, variables) );

	Operation O( (operation_fn)B->op, B->lib_name+":"+B->func_name);

	return expression_ref{O, args};
    }
    else if (auto C = E.to_constant())
    {
	if (auto c = to<char>(C->value))
	    return *c;
	else if (auto i = to<int>(C->value))
	    return *i;
	else if (auto i = to<integer_container>(C->value))
	    return Integer(i->i);
	else if (auto s = to<std::string>(C->value))
	    return String(*s);
	else
	    std::abort();
    }

    std::abort();
}

expression_ref indexify(const Core2::Exp<>& E)
{
    vector<Core2::Var<>> variables;
    return indexify(E,variables);
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

