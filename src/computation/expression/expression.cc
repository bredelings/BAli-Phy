// #define DEBUG_OPTIMIZE

#include <set>
#include <iterator>
#include <map>
#include <cctype>

#include "expression.H"
#include "computation/module.H"
#include "util/string/convert.H"
#include "computation/operation.H"
#include "computation/operations.H"
#include "substitute.H"
#include "let.H"
#include "case.H"
#include "var.H"
#include "lambda.H"
#include "bool.H"
#include "modifiable.H"
#include "reg_var.H"

using std::vector;
using std::string;
using std::set;
using std::pair;
using std::multiset;

using boost::dynamic_pointer_cast;

/* Legal terms are:

   T,U,V -> x
   -> Lx.T
   -> U T
   -> c U[]
   -> let {x=U} in T
   -> case U of {c x[i] -> V[i]}
*/

/* The normalization rules are:

   1. (x)* -> x
   2. (Lx.T)* -> Lx.(T)*
   3. (U T)* -> let x = (T)* in (U)* x , x fresh
   4. (c U[i]) -> let x[i] = (U[i])* in c x[i], x fresh
   5. (let {x[i] = U[i]} in T)* -> let {x=(U[i])*} in (T)*
   6. (case U of {c[i] x[i][] -> T[i]})* -> case (U)* of {c[i] x[i][] -> (T[i])*}
*/

/*
  x -> var[index]
  Lx.T ->(lambda[index] T)
  (U T) -> (U T)
  (c U[i]) -> (c U[i])
  (let {x[i] = U[i]} in T) -> (let [(x[i],U[i])] T)
  (case T in {c[i] x[i] -> U[i]}) -> (case T [(c[i] x[i],U[i])]
*/

/*
 *  Perhaps switch to (lambda var E) instead of (lambda[index] E)
 */ 


// Def: a redex is an expression that matches the LHS of a reduction rule.

// NF = the expression contains no redexes.

// HNF = an expression that is either:
//  * a variable
//  * a data value
//  * a built-in function applied to too few arguments
//  * a lambda abstraction whose body is not reducible.
// An expression in HNF may contain redexes in argument positions whereas a NF may not.
// - but how?

// WHNF = Weak head normal form. WHNF terms have no restriction on the body of lambdas, and so include:
//  * a variable
//  + a data value
//  + a built-in Operation (like "+") with too few arguments.
//  * a lambda expression
//  + a constructor
// The terms in + are extensions to the basic lambda calculus.
// Question: how about the expression (@ x y), which cannot be reduced?

// Basically, HNF requires that the body of a lambda is reduced as well, while WHNF does not have this requirement.
// Therefore, \x -> 1+1 is WHNF but not HNF.

bool is_WHNF(const expression_ref& E)
{
    assert(E);

    int type = E.head().type();

    if (E.size())
    {
	if (type == lambda_type or type == lambda2_type or type == constructor_type) 
	    return true;
	else
	    return false;
    }
    else
    {
	if (is_reglike(E) or type == let2_type)
	    return false;
	else
	    return true;
    }
}

bool is_reglike(const expression_ref& E)
{
    return is_var(E) or is_reg_var(E) or E.is_index_var();
}

expression_ref launchbury_unnormalize(const expression_ref& E)
{
    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	expression* V = E.as_expression().clone();
	V->sub[1] = launchbury_unnormalize(E.sub()[1]);

	if (V->sub[1].ptr() == E.sub()[1].ptr())
	    return E;
	else
	    return V;
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Unormalize the object
	object = launchbury_unnormalize(object);

	const int L = patterns.size();
	// Just unnormalize the bodies
	for(int i=0;i<L;i++)
	    bodies[i] = launchbury_unnormalize(bodies[i]);
    
	return make_case_expression(object, patterns, bodies);
    }

    // 5. Let 
    if (is_let_expression(E))
    {
	vector<pair<var,expression_ref>> decls;
	expression_ref body;
	parse_let_expression(E, decls, body);

	// unnormalize body and the decls
	body = launchbury_unnormalize(body);
	for(auto& decl: decls)
	    decl.second = launchbury_unnormalize(decl.second);

	/*
	  Identify cycles of size > 1...
	  But what is the optimal behavior in that case?
	  matrix<int> U(vars.size(), vars.size());
	  for(int i=0;i<vars.size();i++)
	  {
	  std::set<var> free = get_free_indices(bodies[i]);
	  for(int j=0;j<vars.size();j++)
	  if (free.find(vars[j]))
	  U(i,j) = 0;
	  }
	*/

	// substitute for non-recursive lets
	bool changed = true;
	while(changed)
	{
	    changed = false;

	    for(int i=decls.size()-1; i>=0; i--)
	    {
		auto [x,E] = decls[i];
		std::set<var> free = get_free_indices(E);

		// if x references itself then don't substitute it.
		if (free.count(x)) continue;
	
		changed = true;
	
		decls.erase(decls.begin() + i);
	
		// substitute for the value of this variable in T and in the remaining bodies;
		for(auto& [x2,E2]: decls)
		    E2 = substitute(E2, x, E);
		body = substitute(body, x, E);
	    }
	}

	return let_expression(decls, body);
    }
    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    else if (not E.size() or is_modifiable(E))
	return E;
    // 4. Constructor
    else if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = launchbury_unnormalize(E.sub()[i]);
	return V;
    }

    std::cerr<<"I don't recognize expression '"+ E.print() + "'\n";
    return E;
}

expression_ref parse_object(const string& s)
{
    if (auto int_value = can_be_converted_to<int>(s))
	return *int_value;
    else if (auto double_value = can_be_converted_to<double>(s))
	return *double_value;
    else if (auto bool_value = can_be_converted_to<bool>(s))
    {
	if (*bool_value)
	    return bool_true;
	else
	    return bool_false;
    }
    else if (s.size() >= 2 and s[0] == '"' and s[s.size()-1] == '"')
	return String(s.substr(1,s.size()-2));
    else
	throw myexception()<<"Can't convert '"<<s<<"' to bool, int, double, or string!";
}
