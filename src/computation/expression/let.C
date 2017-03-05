#include "dummy.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "substitute.H"
#include "expression.H"
#include "computation/operations.H"

using std::vector;
using std::string;

tribool let_obj::compare(const Object& o) const 
{
    const let_obj* T = dynamic_cast<const let_obj*>(&o);
    return T;
}

string let_obj::print() const 
{
    return "let";
}

expression_ref indexed_let_expression(const vector<expression_ref>& bodies, const expression_ref& T)
{
    expression* E = new expression( Let() );

    E->sub.push_back(T);

    for(const auto& body: bodies)
	E->sub.push_back(body);

    return E;
}

expression_ref let_expression(const vector<expression_ref>& vars, const vector<expression_ref>& bodies, const expression_ref& T)
{
    if (vars.size() == 0) return T;

    // We COULD merge with T if it is already a let expression, but
    // (a) We'd have to check that no variables overlap, and
    // (b) Sometimes sequential let's execute faster.
    // (c) Let's will probably be merged anyway during let-float.

    expression* E = new expression( let_obj() );
    E->sub.push_back(T);

    for(int i=0;i<vars.size();i++)
    {
	E->sub.push_back(vars[i]);
	E->sub.push_back(bodies[i]);
    }

    return E;
}

expression_ref let_expression(const expression_ref& var, const expression_ref& body, const expression_ref& T)
{
    return let_expression(vector<expression_ref>{var}, vector<expression_ref>{body}, T);
}

//let [(x[i], bodies[i])] T
bool parse_let_expression(const expression_ref& E, vector<expression_ref>& vars, vector<expression_ref>& bodies, expression_ref& T)
{
    vars.clear();
    bodies.clear();

    if (E.head().type() != let_type) return false;

    // There should be an odd number of arguments.
    assert(E.size()%2 == 1);

    T = E.sub()[0];
    const int L = (E.size()-1)/2;
    for(int i=0;i<L;i++)
    {
	vars.push_back(E.sub()[1+2*i]);
	bodies.push_back(E.sub()[2+2*i]);
    }

    return true;
}

//let T bodies[i]
bool parse_indexed_let_expression(const expression_ref& E, vector<expression_ref>& bodies, expression_ref& T)
{
    bodies.clear();

    if (E.head().type() != let2_type) return false;

    T = E.sub()[0];
    const int L = E.size()-1;
    bodies.resize(L);
    for(int i=0;i<L;i++)
	bodies[i] = E.sub()[i+1];

    return true;
}

int n_free_occurrences(const expression_ref& E1, const expression_ref& D)
{
    assert(not is_wildcard(D));

    // If this is the relevant dummy, then substitute
    if (E1.size() == 0)
    {
	if (E1 == D)
	    return 1;
	// If this is any other constant, then it doesn't contain the dummy
	else
	    return 0;
    }

    // Handle case expressions differently
    {
	expression_ref T;
	vector<expression_ref> patterns;
	vector<expression_ref> bodies;
	if (parse_case_expression(E1,T,patterns,bodies))
	{
	    int count = n_free_occurrences(T, D);

	    const int L = (E1.size()-1)/2;

	    for(int i=0;i<L;i++)
	    {
		// don't substitute into subtree where this variable is bound
		std::set<dummy> bound = get_free_indices(patterns[i]);

		bool D_is_bound = false;
		for(const auto& b: bound)
		    if (D == b) D_is_bound=true;

		if (not D_is_bound)
		    count += n_free_occurrences(bodies[i], D);
	    }

	    return count;
	}
    }

    // What indices are bound at the top level?
    std::set<dummy> bound = get_bound_indices(E1);

    // Don't substitute into local variables
    for(const auto& b: bound)
	if (D == b) return 0;
    
    // Since this is an expression, count occurrences in sub-expressions
    int count = 0;
    for(int i=0;i<E1.size();i++)
	count += n_free_occurrences(E1.sub()[i], D);

    return count;
}

expression_ref unlet(const expression_ref& E)
{
    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size() or is_modifiable(E))
	return E;
  
    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	expression* V = E.as_expression().clone();
	V->sub[1] = unlet(E.sub()[1]);

	if (V->sub[1].ptr() == E.sub()[1].ptr())
	    return E;
	else
	    return V;
    }

    // 6. Case
    if (E.head().is_a<Case>())
    {
	expression* V = E.as_expression().clone();

	// Unormalize the object
	V->sub[0] = unlet(V->sub[0]);

	const int L = (V->sub.size() - 1)/2;
	// Just unnormalize the bodies
	for(int i=0;i<L;i++)
	    V->sub[2+2*i] = unlet(V->sub[2+2*i]);
    
	return V;
    }

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = unlet(E.sub()[i]);
	return V;
    }

    // 5. Let 
    if (E.head().is_a<let_obj>())
    {
	vector<expression_ref> vars;
	vector<expression_ref> bodies;
	expression_ref T;
	parse_let_expression(E, vars, bodies, T);

	// unnormalize T and the bodies
	T = unlet(T);
	for(int i=0; i<vars.size(); i++)
	    bodies[i] = unlet(bodies[i]);

	// substitute for non-recursive lets
	bool changed = true;
	while(changed)
	{
	    changed = false;

	    for(int i=vars.size()-1; i>=0; i--)
	    {
		assert(vars[i].is_a<dummy>());

		if (n_free_occurrences(bodies[i],vars[i])) continue;

		int count = n_free_occurrences(T, vars[i]);
		for(const auto& b: bodies)
		    count += n_free_occurrences(b, vars[i]);

		if (count != 1) continue;

		changed = true;
	
		expression_ref var = vars[i];
		expression_ref body = bodies[i];
	
		vars.erase(vars.begin() + i);
		bodies.erase(bodies.begin() + i);
	
		// substitute for the value of this variable in T and in the remaining bodies;
		for(int j=0;j<vars.size();j++)
		    bodies[j] = substitute(bodies[j], var, body);
		T = substitute(T, var, body);
	    }
	}

	return let_expression(vars, bodies, T);
    }

    std::cerr<<"I don't recognize expression '"+ E.print() + "'\n";
    return E;
}

