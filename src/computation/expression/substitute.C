#include <set>
#include "substitute.H"
#include "expression.H"
#include "let.H"
#include "case.H"
#include "dummy.H"
#include "lambda.H"

using std::vector;
using std::string;
using std::set;
using std::multiset;

// Return the list of dummy variable indices that are bound at the top level of the expression
void alpha_rename(object_ptr<expression>& E, const expression_ref& x, const expression_ref& y)
{
    // assert: x is bound in E
    // assert: y is neither bound in E nor free in E

    // std::cout<<" replacing "<<x<<" with "<<y<<" in "<<E->print()<<":\n";
    // Make sure we don't try to substitute for lambda-quantified dummies
    if (E->head.is_a<lambda>())
    {
	assert(E->sub[0] == x);
	E->sub[0] = y;
	E->sub[1] = substitute(E->sub[1], x, y);
    }
    else if (E->head.is_a<let_obj>())
    {
	for(int i=0;i<E->size();i++)
	    E->sub[i] = substitute(E->sub[i], x, y);
    }
    else
	assert(false);
    // std::cout<<"    "<<E->print()<<"\n";
}


// If we use de Bruijn indices, then, as before bound indices in R2 are no problem.
// If we do NOT use de Bruijn indices for FREE variables, then we don't have to adjust them when we substitute.

// Current problem: we need a generic way to represent which indices are bound.

// For de bruijn indices in case expressions, we could specify (?:?):(_:?) where ? means "keep" and "_" means not to keep.
// When the user specifies such a pattern we could number the ? variables on the horizontal level.

// We could also number individual variables in letrec expressions horizontally.

// However, how do we add names back in when we want to print them?


// Idea: switch to de bruijn indices for bound variables only.  Makes substitution much simpler!
// Question: how would I encode names?

bool do_substitute(expression_ref& E1, const expression_ref& D, const expression_ref& E2)
{
#ifndef NDEBUG
    expression_ref orig = E1;
#endif
    assert(not is_wildcard(D));

    // If this is the relevant dummy, then substitute
    if (E1.size() == 0)
    {
	if (E1 == D)
	{
	    E1 = E2;
	    return true;
	}
	// If this is any other constant, then it doesn't contain the dummy
	else
	    return false;
    }

    // Handle case expressions differently
    {
	expression_ref T;
	vector<expression_ref> patterns;
	vector<expression_ref> bodies;
	bool changed = false;
	if (parse_case_expression(E1,T,patterns,bodies))
	{
	    changed = do_substitute(T, D, E2) or changed;

	    for(int i=0;i<patterns.size();i++)
	    {
		// 1. don't substitute into subtree where this variable is bound
		std::set<dummy> bound = get_free_indices(patterns[i]);

		bool D_is_bound = false;
		for(const auto& b: bound)
		    if (D == b) D_is_bound=true;
		if (D_is_bound) continue;

		// 2. If some of the free variables in E2 are bound in patterns[i], then do 
		// alpha-renaming on (patterns[i],bodies[i]), to avoid name capture.

		std::set<dummy> fv2 = get_free_indices(E2);
		std::set<dummy> overlap = intersection(bound,fv2);
    
		if (not overlap.empty())
		{
		    // Determine the free variables of {patterns[i],bodies[i]} so that we can avoid them in alpha renaming
		    std::set<dummy> fv1 = get_free_indices(bodies[i]);
		    for(const auto& b: bound)
			fv1.erase(b);
	  
		    // If bodies[i] does not contain D, we won't do any substitution anyway, so avoid alpha renaming.
		    // Since D is not bound by patterns, we just need to check if D is in fv1 = fv(body)-fv(pattern).
		    if (D.is_a<dummy>())
		    {
			if (fv1.find(D.as_<dummy>()) == fv1.end()) continue;
		    }
	  
		    // Compute the total set of free variables to avoid clashes with when alpha renaming.
		    add(fv2, fv1);
	  
		    // we don't want to rename on top of any other variables bound here
		    int new_index = std::max(max_index(fv2),max_index(bound))+1;
	  
		    // Do the alpha renaming
		    for(const auto& o:overlap) 
		    {
			patterns[i] = substitute(patterns[i], dummy(o), dummy(new_index));
			bodies[i] = substitute(bodies[i], dummy(o), dummy(new_index));
			new_index++;
		    }
		    changed = true;
	  
		    // We rename a bound variable dummy(i) in patterns[i]/bodies[i] that is free in E2 to a new variable dummy(new_index)
		    //   that is not bound or free in the initial version of patterns[i]/bodies[i] and free in E2.
	  
		    // The conditions are therefore:
		    //   dummy(*i) must be bound in patterns[i]
		    //   dummy(new_index) must be neither bound nor free in E1
		    //   dummy(new_index) must not be free in E2
		}

		// assert that D contains no free variables that are bound in patterns[i]
		changed = (do_substitute(bodies[i], D, E2) or changed);
	    }

	    if (changed)
		E1 = make_case_expression(T, patterns, bodies);

	    return changed;
	}
    }

    // What indices are bound at the top level?
    std::set<dummy> bound = get_bound_indices(E1);

    bool changed = false;
    if (not bound.empty())
    {
	// Don't substitute into local variables
	for(const auto& b: bound)
	    if (D == b) return false;
    
	std::set<dummy> fv2 = get_free_indices(E2);
	std::set<dummy> overlap = intersection(bound,fv2);
    
	// If some of the free variables in E2 are bound in E1, then do alpha-renaming on E1 to avoid name capture.
	if (not overlap.empty())
	{
	    // Determine the free variables of E1 so that we can avoid them in alpha renaming
	    std::set<dummy> fv1 = get_free_indices(E1);

	    // If E1 does not contain D, then we won't do any substitution anyway, so avoid alpha renaming.
	    if (D.is_a<dummy>())
		if (fv1.find(D.as_<dummy>()) == fv1.end()) return false;

	    // Compute the total set of free variables to avoid clashes with when alpha renaming.
	    add(fv2, fv1);

	    // we don't want to rename on top of any other variables bound here
	    int new_index = std::max(max_index(fv2),max_index(bound))+1;

	    // Do the alpha renaming
	    object_ptr<expression> E1_ (E1.as_expression().clone());
	    for(const auto& i:overlap)
		alpha_rename(E1_, dummy(i), dummy(new_index++));
	    E1 = E1_;
	    changed = true;

	    // We rename a bound variable dummy(i) in E1 that is free in E2 to a new variable dummy(new_index)
	    //   that is not bound or free in the initial version of E1 and free in E2.

	    // The conditions are therefore:
	    //   dummy(*i) must be bound in E1
	    //   dummy(new_index) must be neither bound nor free in E1
	    //   dummy(new_index) must not be free in E2
	}
    }

    // Since this is an expression, substitute into sub-expressions
    object_ptr<expression> E1_ (E1.as_expression().clone());
    for(int i=0;i<E1_->size();i++)
	changed = (do_substitute(E1_->sub[i], D, E2) or changed);

    if (changed)
	E1 = E1_;

    assert((E1.ptr() != orig.ptr()) == changed);
    return changed;
}

expression_ref substitute(const expression_ref& R1, int dummy_index, const expression_ref& R2)
{
    return substitute(R1,dummy(dummy_index),R2);
}

expression_ref substitute(const expression_ref& R1, const expression_ref& D, const expression_ref& R2)
{
    expression_ref R1b = R1;
    do_substitute(R1b, D, R2);
    return R1b;
}



