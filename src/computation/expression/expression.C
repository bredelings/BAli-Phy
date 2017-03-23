// #define DEBUG_OPTIMIZE

#include <set>
#include <iterator>
#include <map>
#include <cctype>

#include "expression.H"
#include "computation/module.H"
#include "util.H"
#include "computation/operation.H"
#include "computation/operations.H"
#include "computation/graph_register.H"
#include "substitute.H"
#include "let.H"
#include "case.H"
#include "dummy.H"
#include "lambda.H"

using std::vector;
using std::string;
using std::set;
using std::pair;
using std::multiset;

using boost::dynamic_pointer_cast;

// 1. factor out: list, tuple
// Also try to simplify the apply_subst stuff in tuple & list files
// 2. eliminate substitution.H
// 3. Eliminate identifier in favor of dummy (==var)?
// 4. Remove horrible (#symbol)*(#function) substitution in module.C

identifier::identifier(const std::string& s)
    :name(s)
{
    assert(not name.empty());
}

tribool parameter::compare(const Object& o) const 
{
    const parameter* E = dynamic_cast<const parameter*>(&o);
    if (not E) 
	return false;

    return parameter_name == E->parameter_name;
}

parameter::parameter(const std::string& s)
    :parameter_name(s)
{
}

/// 1. Hey, could we solve the problem of needing to rename dummies by doing capture-avoiding substitution?
/// I think we could!
///
/// Suppose we have Lf.Lx.fx, and we apply it to Lx.x (the identity function), then we get
///    (Lf.Lx.fx)(Lx.x) = (Lx.fx)[f := Lx.x] = Lx.(Lx.x)x.
/// Then, if we apply this to y, we get
///    (Lx.(Lx.x)x)y = (Lx.x)x[x := y] = (Lx.x)y 
/// And
///    (Lx.x)y = y;
///
/// 2. However, is sometimes still necessary to rename dummies.  This is true if E2 contains unbound dummies
///    that are bound in E1.
///
///    For example, apply Lx.y to x, then we would get Lx.x, which is not allowed.
///    Instead, we must "alpha-convert" Lx.y to Lz.y, and then apply Lz.y to x, leading to Lz.x .

/// Literally E2 for D in E1. (e.g. don't rename variables in E2).  Throw an exception if D is a lambda-bound dummy variable.

void find_named_parameters(const expression_ref& E, std::set<string>& names)
{
    assert(E);
    // If this is a parameter, then makes sure we've got its name.
    if (E.is_a<parameter>())
    {
	auto& n = E.as_<parameter>();
	assert(not E.size());
	if (names.find(n.parameter_name) == names.end())
	    names.insert(n.parameter_name);
    }

    // Check the sub-objects of this expression.
    for(int i=0;i<E.size();i++)
	find_named_parameters(E.sub()[i], names);
}

set<string> find_named_parameters(const expression_ref& e)
{
    set<string> names;
    find_named_parameters(e, names);
    return names;
}

set<string> find_named_parameters(const vector<expression_ref>& notes)
{
    set<string> names;
    for(int i=0;i<notes.size();i++)
	find_named_parameters(notes[i], names);
    return names;
}

expression_ref add_prefix(const string& prefix, const expression_ref& E)
{
    std::set<string> names = find_named_parameters(E);

    expression_ref E2 = E;
    for(const auto& name: names)
	E2 = substitute(E2, parameter(name), parameter(prefix+"."+name));

    return E2;
}

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
  x -> dummy[index]
  Lx.T ->(lambda[index] T)
  (U T) -> (U T)
  (c U[i]) -> (c U[i])
  (let {x[i] = U[i]} in T) -> (let [(x[i],U[i])] T)
  (case T in {c[i] x[i] -> U[i]}) -> (case T [(c[i] x[i],U[i])]
*/

/*
 *  Perhaps switch to (lambda dummy E) instead of (lambda[index] E)
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
	if (is_reglike(E))
	    return false;
	else
	    return true;
    }
}

bool is_parameter(const expression_ref& E)
{
    return E.is_a<parameter>();
}

bool is_modifiable(const expression_ref& E)
{
    bool result = E.head().type() == modifiable_type;
    assert(result == E.head().is_a<modifiable>());
    return result;
}

bool is_identifier(const expression_ref& E)
{
    return E.is_a<identifier>();
}

bool is_reg_var(const expression_ref& E)
{
    return E.is_a<reg_var>();
}

bool is_reglike(const expression_ref& E)
{
    return is_dummy(E) or is_parameter(E) or is_modifiable(E) or is_reg_var(E) or E.is_index_var() or is_identifier(E);
}

expression_ref launchbury_unnormalize(const expression_ref& E)
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
	V->sub[1] = launchbury_unnormalize(E.sub()[1]);

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
	V->sub[0] = launchbury_unnormalize(V->sub[0]);

	const int L = V->sub.size()/2 - 1;
	// Just unnormalize the bodies
	for(int i=0;i<L;i++)
	    V->sub[2+2*i] = launchbury_unnormalize(V->sub[2+2*i]);
    
	return V;
    }

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = launchbury_unnormalize(E.sub()[i]);
	return V;
    }

    // 5. Let 
    if (is_let_expression(E))
    {
	vector<pair<dummy,expression_ref>> decls;
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
	  std::set<dummy> free = get_free_indices(bodies[i]);
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
		auto& x = decls[i].first;
		std::set<dummy> free = get_free_indices(decls[i].second);

		// if x references itself then don't substitute it.
		if (free.count(x)) continue;
	
		changed = true;
	
		auto decl = decls[i];
		decls.erase(decls.begin() + i);
	
		// substitute for the value of this variable in T and in the remaining bodies;
		for(int j=0;j<decls.size();j++)
		    decls[j].second = substitute(decls[j].second, decl.first, decl.second);
		body = substitute(body, decl.first, decl.second);
	    }
	}

	return let_expression(decls, body);
    }

    std::cerr<<"I don't recognize expression '"+ E.print() + "'\n";
    return E;
}

expression_ref parse_object(const string& s)
{
    bool bool_value;
    int int_value;
    double double_value;

    if (can_be_converted_to<int>(s, int_value))
	return int_value;
    else if (can_be_converted_to<double>(s, double_value))
	return double_value;
    else if (can_be_converted_to<bool>(s, bool_value))
    {
	if (bool_value)
	    return constructor("Prelude.True",0);
	else
	    return constructor("Prelude.False",0);
    }
    else if (s.size() >= 2 and s[0] == '"' and s[s.size()-1] == '"')
	return String(s.substr(1,s.size()-2));
    else
	throw myexception()<<"Can't convert '"<<s<<"' to bool, int, double, or string!";
}

vector<double> vec_to_double(const std::vector<expression_ref>& v)
{
    vector<double> vv(v.size());
    for(int i=0;i<v.size();i++)
	vv[i] = v[i].as_double();
    return vv;
}

vector<int> vec_to_int(const std::vector<expression_ref>& v)
{
    vector<int> vv(v.size());
    for(int i=0;i<v.size();i++)
	vv[i] = v[i].as_int();
    return vv;
}
