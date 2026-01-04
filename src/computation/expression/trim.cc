#include "trim.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "index_var.H"
#include "constructor.H"
#include "computation/operations.H"

using std::vector;
using std::string;

string Trim::print() const
{
    return "Trim";
}

bool Trim::operator==(const Object& o) const 
{
    return dynamic_cast<const Trim*>(&o);
}

vector<int> pop_vars(int n, vector<int> vars)
{
    assert(n >= 0);
    if (n == 0) return vars;

    for(int& var: vars)
	var -= n;
    while(vars.size() and vars[0] < 0)
	vars.erase(vars.begin());
    return vars;
}

vector<int> merge_vars(const vector<int>& v1, const vector<int>& v2)
{
    int i=0;
    int j=0;
    vector<int> v3;
    while(i<v1.size() or j<v2.size())
    {
	if (i >= v1.size())
	    v3.push_back(v2[j++]);
	else if (j >= v2.size())
	    v3.push_back(v1[i++]);
	else if (v1[i] < v2[j])
	    v3.push_back(v1[i++]);
	else if (v1[i] > v2[j])
	    v3.push_back(v2[j++]);
	else {
	    assert(v1[i] == v2[j]);
	    v3.push_back(v1[i]);
	    i++; j++;
	}
    }
    assert(v3.size() >= v1.size());
    assert(v3.size() >= v2.size());
    return v3;
}

vector<int> get_free_index_vars(const expression_ref& E)
{
    vector<int> vars;
  
    if (E.head().is_a<Trim>())
    {
	// Which vars are we not throwing away?
	// This should also be an assert.
	vars = E.sub()[0].as_<Vector<int>>();
    
#ifndef NDEBUG
	vector<int> vars2 = get_free_index_vars(E.sub()[1]);
	assert(vars.size() == vars2.size());
	for(int i=0;i<vars.size();i++)
	    assert(vars2[i] == i);
#endif
    }
    // Lambda expression - /\x.e
    else if (E.head().is_a<lambda2>())
	vars = pop_vars(1, get_free_index_vars(E.sub()[0]));

    // Let expression
    else if (auto L = parse_indexed_let_expression(E))
    {
	vars = get_free_index_vars(L->body);

	for(const auto& bind: L->binds)
	    vars = merge_vars(vars, get_free_index_vars(bind));

	vars = pop_vars(L->binds.size(), vars);
    }

    // case expression
    else if (auto case_exp = parse_case_expression(E))
    {
        auto& [object, alts] = *case_exp;
	vars = get_free_index_vars(object);

	for(auto& [pattern, body]: alts)
	{
	    int n = 0;

	    // Handle c[i] x[i][1..n] -> body[i]
	    if (pattern.head().is_a<constructor>())
		n = pattern.head().as_<constructor>().n_args();

	    vars = merge_vars(vars, pop_vars(n, get_free_index_vars(body)) );
	}
    }
    else if (not E.size()) 
    {
	// Variable
	if (E.is_index_var())
	    return {E.as_index_var()};
	// Constant
	else
	    return vector<int>{};
    }
    else
    {
	for(int i=0;i<E.size();i++)
	    vars = merge_vars(vars, get_free_index_vars(E.sub()[i]) );
    }

    //  std::cerr<<"fv("<<R<<"): "<<join(vars,",")<<"\n";

    return vars;
}

expression_ref trim_normalize(const expression_ref& E)
{
    vector<int> vars;
  
    // Let expressions need to be normalized
    if (auto L = parse_indexed_let_expression(E))
    {
	L->body = trim(trim_normalize(L->body));

	for(auto& bind: L->binds)
	    bind = trim(trim_normalize(bind));

	return *L;
    }

    // case expression
    else if (auto case_exp = parse_case_expression(E))
    {
        auto& [object, alts] = *case_exp;

	// object should not contain any lets or case-alts, so don't bother about it.

	for(auto& [pattern, body]: alts)
	    body = trim(trim_normalize(body));

	return make_case_expression(object, alts);
    }
    // Already normalized (though not trimmed)
    else if (not E.size()) return E;

    else
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = trim_normalize(V->sub[i]);

	return V;
    }
}

expression_ref make_trim(const expression_ref& E, const vector<int>& indices)
{
#ifndef NDEBUG
    vector<int> vars = get_free_index_vars(E);
    for(int i=0;i<vars.size();i++)
	assert(vars[i] == i);
    assert(indices.size() == vars.size());
#endif

    expression* V = new expression(Trim());
    V->sub.push_back(Vector<int>(indices));
    V->sub.push_back(E);

    return V;
}

expression_ref remap_free_indices(const expression_ref& E, const vector<int>& mapping, int depth)
{
    vector<int> vars;
  
    if (E.head().is_a<Trim>())
    {
	// Which vars are we not throwing away?
	// This should also be an assert.
	vars = E.sub()[0].as_<Vector<int>>();

	// remap free vars
	for(auto& var:vars)
	{
	    int delta = var - depth;
	    if (delta >= 0)
	    {
		assert(delta < mapping.size());
		assert(mapping[delta] != -1);

		var = depth + mapping[delta];
	    }
	}
    
#ifndef NDEBUG
	vector<int> vars2 = get_free_index_vars(E.sub()[1]);
	assert(vars.size() == vars2.size());
	for(int i=0;i<vars.size();i++)
	    assert(vars2[i] == i);
#endif

	return make_trim(E.sub()[1], vars);

    }
    // Lambda expression - /\x.e
    else if (E.head().is_a<lambda2>())
    {
	expression* V = new expression(lambda2());
	V->sub.push_back(remap_free_indices(E.sub()[0], mapping, depth+1));
	return V;
    }

    // Let expression
    else if (auto L = parse_indexed_let_expression(E))
    {
	int n = L->binds.size();
	L->body = remap_free_indices(L->body, mapping, depth + n);

	for(auto& bind: L->binds)
	    bind = remap_free_indices(bind, mapping, depth + n);

	return *L;
    }
  
    // case expression
    else if (auto case_exp = parse_case_expression(E))
    {
        auto& [object, alts] = *case_exp;
	object = remap_free_indices(object, mapping, depth);

	for(auto& [pattern, body]: alts)
	{
	    int n = 0;

	    // Handle c[i] x[i][1..n] -> body[i]
	    if (pattern.head().is_a<constructor>())
		n = pattern.head().as_<constructor>().n_args();

	    body = remap_free_indices(body, mapping, depth + n);
	}

	return make_case_expression(object, alts);
    }
    else if (not E.size())
    {
	// Variable
	if (E.is_index_var())
	{
	    int index = E.as_index_var();
	    int delta = index - depth;
	    if (delta >= 0)
	    {
		assert(delta < mapping.size());
		assert(mapping[delta] != -1);

		return index_var(depth + mapping[delta]);
	    }
	    else
		// Var that is to new to be remapped.
		return E;
	}
	// Constant
	else
	    return E;
    }
    else
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = remap_free_indices(V->sub[i], mapping, depth);
	return V;
    }
}

expression_ref trim(const expression_ref& E)
{
    // Well, it would seem that the relevant matter is that we are at depth n.
    vector<int> indices = get_free_index_vars(E);

    vector<int> mapping;

    if (indices.size())
    {
	mapping = vector<int>(indices.back()+1, -1);
	for(int i=0;i<indices.size();i++)
	    mapping[indices[i]] = i;
    }

    return make_trim( remap_free_indices(E, mapping, 0), indices);
}

expression_ref untrim(const expression_ref& E)
{
    if (E.head().is_a<Trim>())
	return remap_free_indices(E.sub()[1], E.sub()[0].as_<Vector<int>>(), 0);
    else
	return E;
}

// This only removes trimmers from the places that trim_normalize puts them.
// (Since remap_free_indices( ) doesn't enter trimmers, this should be relatively efficient,
//  just like trim_normalize( ))
expression_ref trim_unnormalize(const expression_ref& E)
{
    vector<int> vars;
  
    // Let expressions need to be normalized
    if (auto L = parse_indexed_let_expression(E))
    {
	L->body = trim_unnormalize(untrim(L->body));

	for(auto& bind: L->binds)
	    bind = trim_unnormalize(untrim(bind));

	return *L;
    }

    // case expression
    else if (auto case_exp = parse_case_expression(E))
    {
        auto& [object, alts] = *case_exp;
	// object should not contain any lets or case-alts, so don't bother about it.

	for(auto& [pattern, body]: alts)
	    body = trim_unnormalize(untrim(body));

        return make_case_expression(object, alts);
    }
    // Already normalized (though not trimmed)
    else if (not E.size()) return E;

    else
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<V->size();i++)
	    V->sub[i] = trim_unnormalize(untrim(V->sub[i]));

	return V;
    }
}

