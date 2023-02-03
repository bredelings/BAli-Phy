#include "indexify.H"
#include "constructor.H"
#include "var.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "index_var.H"
#include "apply.H"

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
    if (is_let_expression(E))
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
    expression_ref T;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, T, patterns, bodies))
    {
	T = indexify(T, variables);

	for(int i=0;i<bodies.size();i++)
	{
	    // Handle c[i] x[i][1..n] -> body[i]
	    expression_ref& P = patterns[i];
	    expression_ref& B = bodies[i];

#ifndef NDEBUG
	    // FIXME - I guess this doesn't handle case a of b -> f(b)?
	    if (is_var(P))
		assert(is_wildcard(P));
#endif

	    for(int j=0;j<P.size();j++)
		variables.push_back(P.sub()[j].as_<var>());

	    B = indexify(B, variables);

	    for(int j=0;j<P.size();j++)
		variables.pop_back();

	    P = P.head();
	}

	return make_case_expression(T, patterns, bodies);
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
            expression* V = E.as_expression().clone();
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
    vector<expression_ref> bodies;
    vector<pair<var,expression_ref>> decls;
    expression_ref body;
    if (parse_indexed_let_expression(E, bodies, body))
    {
	vector<expression_ref> variables2 = variables;
	for(int i=0;i<bodies.size();i++)
	{
	    var d = get_named_var(variables2.size());
	    decls.push_back({d, bodies[i]});
	    variables2.push_back( d );
	}

	// Deindexify let-bound stmts only after list of variables has been extended.
	for(int i=0;i<decls.size();i++)
	    decls[i].second = deindexify(decls[i].second, variables2);

	body = deindexify(body, variables2);

	return let_expression(decls, body);
    }

    // case expression
    vector<expression_ref> patterns;
    expression_ref T;
    if (parse_case_expression(E, T, patterns, bodies))
    {
	T = deindexify(T, variables);

	for(int i=0;i<bodies.size();i++)
	{
	    assert(not patterns[i].size());
	    // Make a new expression so we can add variables to the pattern if its a constructor
	    expression_ref P = patterns[i];
	    expression_ref& B = bodies[i];

	    // Find the number of arguments in the constructor
	    int n_args = 0;
	    if (P.head().is_a<constructor>())
		n_args = P.head().as_<constructor>().n_args();

	    // Add n_arg variables to the stack and to the pattern
	    vector<expression_ref> variables2 = variables;
	    for(int j=0;j<n_args;j++)
	    {
		var d = get_named_var(variables2.size());
		variables2.push_back( d );
		P = P + d;
	    }

#ifndef NDEBUG
	    if(is_var(P))
		assert(is_wildcard(P));
#endif

	    patterns[i] = P;
	    B = deindexify(B, variables2);
	}

	return make_case_expression(T, patterns, bodies);
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
            expression* V = E.as_expression().clone();
            for(int i=0;i<V->size();i++)
                V->sub[i] = deindexify(V->sub[i], variables);
            return V;
        }
    }

    std::abort();
}

expression_ref deindexify(const expression_ref& E)
{
    return deindexify(E,vector<expression_ref>{});
}

