#include <vector>
#include <set>
#include "let-float.H"
#include "computation/expression/expression.H" // for is_reglike( )
#include "computation/expression/substitute.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/apply.H"
#include "computation/expression/constructor.H"
#include "computation/expression/case.H"
#include "computation/expression/operator.H"
#include "computation/operation.H"
#include "computation/module.H"
#include "util/set.H"

#include "free-vars.H"
#include "immer/map.hpp" // for immer::map

#include "range/v3/all.hpp"
namespace view = ranges::view;
namespace action = ranges::action;

using std::vector;
using std::set;
using std::pair;
using std::string;

// ?- Determine which let statements have bound vars (bound_indices) and which do not (unbound_indices).
//    (The ones with no bound vars can be floated.)
bool find_let_statements_with_bound_vars(const vector<pair<var,expression_ref>>& decls,
					 const set<var>& bound,
					 vector<int>& bound_indices, vector<int>& unbound_indices)
{
    // Find the set of bound variables that could be free in let_bodies
    set<var> visible_bound = bound;
    set<var> let_bound;
    for(int i=0;i<decls.size();i++)
    {
	auto x = decls[i].first;
	let_bound.insert(x);
	visible_bound.erase(x);
    }

    vector< set<var> > free_vars;
    for(int i=0;i<decls.size();i++)
	free_vars.push_back( get_free_indices( decls[i].second ) );

    bound_indices.clear();
    unbound_indices.clear();
    for(int i=0;i<decls.size();i++)
	unbound_indices.push_back(i);

    // Find the indices that are not bound (directly or indirectly) by the bound variables
    set<var> new_bound = visible_bound;
    while (not new_bound.empty())
    {
	set<var> new_bound_next;
	for(int i=unbound_indices.size()-1;i>=0;i--)
	{
	    int index = unbound_indices[i];
	    auto x = decls[index].first;
	    if (not intersection(free_vars[index], new_bound).empty())
	    {
		new_bound_next.insert(x);
		bound_indices.push_back(index);
		unbound_indices.erase( unbound_indices.begin() + i);
	    }
	}
	new_bound = new_bound_next;
    }

    return (not unbound_indices.empty());
}

//question: is move_lets supposed to be called with empty vars?
//answer: yes, sometimes.

/// Given let vars=bodies in (<binder bound> (let E_vars=E_bodies in T)), 
///  move some of the E_vars=E_bodies up to vars=bodies.
expression_ref move_lets(bool scope, const expression_ref E,
			 vector<pair<var,expression_ref>>& decls,
			 const set<var>& bound, const set<var>& free)
{
    assert(E);

    vector<pair<var,expression_ref>> E_decls;
    expression_ref E2 = E;

    if (not parse_let_expression(E, E_decls, E2))
	E2 = E;

    // Find the set of variables to avoid renaming over: free + bound + let-bound-just-above
    //    (Hmm... should the let-bound-just-above be in 'bound'?)
    set<var> avoid = free;
    for(int i=0;i<decls.size();i++)
    {
	var x = decls[i].first;
	avoid.insert(x);
	add(avoid, get_free_indices(decls[i].second));
    }
    add(avoid, get_free_indices(E));
    add(avoid, bound);

    int new_index = max_index(avoid) + 1;
    

    // Determine which of the let-statements in E we can float.
    vector<int> unbound_indices;
    vector<int> bound_indices;
    if (find_let_statements_with_bound_vars(E_decls, bound, bound_indices, unbound_indices))
    {
	// Adjust the new indices to avoid hitting any of the other let-binder-variables in E
	for(int i=0;i<E_decls.size();i++)
	{
	    var x = E_decls[i].first;
	    new_index = std::max(new_index, x.index + 1);
	}

	/******************** alpha-rename E -> EE ********************/
    
	object_ptr<expression> EE = E.as_expression().clone();                  // Make a copy of E that we can alpha-rename.
    
	for(int index: unbound_indices)
	{
	    var D = E_decls[index].first;
	    if (includes(avoid, D))
	    {
		var D2(new_index++);
		assert(not includes(avoid,D2));
		alpha_rename(EE, D, D2);
		avoid.insert(D2);
	    }
	}
  
	// Recompute E_vars and E_bodies from the alpha-renamed version of E
	parse_let_expression(EE, E_decls, E2);            
    
	/*********** move free lets to higher-level environment **********/
	for(int index: unbound_indices)
	{
#ifndef NDEBUG
	    // Check that we aren't duplicating any variables in the higher-level environment
	    //      for(int j=0;j<vars.size();j++)
	    //	assert(not includes(vars, E_vars[index]));
#endif
	    decls.push_back( E_decls[index] );
	}

	// Construct the remainder expression
	vector<pair<var,expression_ref>> E_decls2;
	for(int i=0;i<bound_indices.size();i++)
	{
	    int index = bound_indices[i];
      
	    E_decls2.push_back(E_decls[index]);
	}

	E2 = let_expression(E_decls2, E2);
    }
    // If nothing is moveable, then just return the original statement.
    else
	E2 = E;

    // We can't float this out because its bound, or because there's no bound to float it through.
    if ((not scope) or (not intersection(get_free_indices(E2), bound).empty()))
    {
	assert(E2);
	return E2;
    }

    // Since we only substitute reg_vars into var's (for let, lambda, and case) these are all OK.
    // change to is_reg_like
    if (is_reglike(E2))
    {
	assert(E2);
	return E2;
    }


    // If E2 is not bound, and its not a let-bound var, then create a new expression for it.
    var D2(new_index++);
    decls.push_back({D2, E2});
    return D2;
}

template <typename T>
bool operator==(const std::set<T>& S1, const std::set<T>& S2)
{
    return includes(S1,S2) and includes(S2,S2);
}

// When we let_float \x.\y.x, we should float out x, even though its a var

// However, if we have let {z=2} in \x.\y.z, we should not introduce a let var
// for z, because its already let bound.

expression_ref let_float(const expression_ref& E)
{
    // 0. NULL
    if (not E) return E;

    // 1. Dummy variable
    // 2. Literal constants.  Treat as 0-arg constructor.
    if (not E.size()) return E;
  
    set<var> free_in_E = get_free_indices(E);

    vector<pair<var, expression_ref>> decls;
    vector<expression_ref> patterns;
    vector<expression_ref> branches;
    expression_ref object;
    expression_ref T;
    expression_ref E2;
    
    // 3. Lambda expressions
    if (E.head().is_a<lambda>())
    {
	// Find the new let-bound set.
	var x= E.sub()[0].as_<var>();

	// First float lets in sub-expressions
	expression_ref M = let_float(E.sub()[1]);

	// Determine the bound indices
	set<var> bound = {x};

	// Move lets across the lambda
	M = move_lets(true, M, decls, bound, free_in_E);

	// Reassemble the expression
	E2 = let_expression(decls, lambda_quantify(x, M) );

	assert(free_in_E == get_free_indices(E2));
    }

    // 4. Case expressions
    else if (parse_case_expression(E,object,patterns,branches))
    {
	// First float out of case object (bound = {}, free = fv(E))
	object = let_float(object);
	object = move_lets(true, object, decls, set<var>(), free_in_E);

	for(int i=0;i<branches.size();i++)
	{
	    // Find the bound variables in the i-th constructor
	    set<var> bound = get_free_indices(patterns[i]);

	    // Second float out of the case alternative branches (bound = fv(patterns[i]), free = fv(E))
	    // (Note: free = fv(E) is a bit conservative.)
	    branches[i] = let_float(branches[i]);
	    branches[i] = move_lets(true, branches[i], decls, bound, free_in_E);
	}

	E2 = let_expression(decls, make_case_expression(object, patterns, branches));

	assert(free_in_E == get_free_indices(E2));
    }

    // 5. Let expressions
    else if (parse_let_expression(E,decls,T))
    {
	// Return let_float(T) if T doesn't mention any of the newly let-bound variables
	set<var> bound_vars_let;
	for(int i=0;i<decls.size();i++)
	    bound_vars_let.insert(decls[i].first);

	set<var> free_vars_T = get_free_indices(T);
	if (intersection(bound_vars_let, free_vars_T).empty()) 
	    return let_float(T);

	// First float lets in sub-expressions
	T = let_float(T);
	for(auto& decl: decls)
	    decl.second = let_float(decl.second);

	// Move lets out of T and into vars
	T = move_lets(false, T, decls, set<var>(), free_in_E);

	// Move lets out of bodies and into vars
	for(int i=0;i<decls.size();i++)
	{
	    // Note that bodies[i] might refer to a different object, if bodies is resized during move_lets.
	    expression_ref E = move_lets(false, decls[i].second, decls, set<var>(), free_in_E);
	    // Therefore ensure that bodies[i] refer to the ith element of bodies AFTER any possible resize.
	    decls[i].second = E;
	}

	E2 = let_expression(decls, T);
    }

    // 6. Handle application, constructors, and operations.
    else if (E.head().is_a<Operator>())
    {
	decls.clear();
	// First float lets in sub-expressions
	object_ptr<expression> V = E.as_expression().clone();
    
	// Move lets from arguments into (vars,bodies)
	for(int i=0;i<E.size();i++)
	{
	    V->sub[i] = let_float(V->sub[i]);
	    V->sub[i] = move_lets(true, V->sub[i], decls, set<var>(), free_in_E);
	}
      
	E2 = let_expression(decls, object_ptr<const expression>(V));

	assert(free_in_E == get_free_indices(E2));
    }
    else
	throw myexception()<<"let_float: I don't understand expression '"<<E<<"'";

#ifndef NDEBUG
    set<var> S2 = get_free_indices(E2);
    assert(free_in_E == S2);
#endif

    return E2;
}
