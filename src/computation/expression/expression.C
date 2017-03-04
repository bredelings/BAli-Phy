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
#include "computation/expression/substitute.H"
#include "computation/expression/let.H"
#include "computation/expression/trim.H"
#include "computation/expression/case.H"

using std::vector;
using std::string;
using std::set;
using std::multiset;

using boost::dynamic_pointer_cast;

// 1. factor out: list, tuple
// 2. eliminate substitution.H
// 3. Eliminate identifier in favor of dummy (==var)?
// 4. Remove horrible (#symbol)*(#function) substitution in module.C

bool dummy::operator==(const dummy& d) const
{
    return index == d.index and name == d.name;
}

tribool dummy::compare(const Object& o) const 
{
    const dummy* D = dynamic_cast<const dummy*>(&o);
    if (not D) 
	return false;

    return (*this) == *D;
}

identifier::identifier(const std::string& s)
    :name(s)
{
    assert(not name.empty());
}

string dummy::print() const {
    if (is_wildcard())
	return "_";
    else if (name.size() and index == -1)
	return name;
    else
	return name+string("#")+convertToString(index);
}

bool dummy::operator<(const dummy& D) const 
{
    if (name.size() and not D.name.size())
	return true;

    if (not name.size() and D.name.size())
	return false;

    if (name.size())
	return name < D.name;
    else
	return index < D.index;
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

string lambda::print() const {
    return "lambda";
}

tribool lambda::compare(const Object& o) const 
{
    return dynamic_cast<const lambda*>(&o);
}

string lambda2::print() const {
    return "/\\";
}

tribool lambda2::compare(const Object& o) const 
{
    return dynamic_cast<const lambda2*>(&o);
}

expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& R)
{
    return new expression(lambda(),{dummy, R});
}

expression_ref lambda_quantify(int dummy_index, const expression_ref& R)
{
    return lambda_quantify(dummy(dummy_index), R);
}

expression_ref lambda_expression(const Operator& O)
{
    int n = O.n_args();
    assert(n != -1);
  
    expression_ref R;
    if (n == 0)
	R = expression_ref(O.clone());
    else
    {
	expression* E = new expression(O);
	for(int i=0;i<n;i++)
	    E->sub.push_back(expression_ref(dummy(i)));
	R = expression_ref(E);
    }
  
    for(int i=n-1;i>=0;i--) 
	R = lambda_quantify(i,R);
  
    return R;
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

std::set<dummy> get_free_indices(const expression_ref& E);

/// Return the min of v
template<typename T>
T max(const std::set<T>& v)
{
    T t = *v.begin();
    for(const auto& i: v)
	t = std::max(t,i);

    return t;
}

int max_index(const std::set<dummy>& s)
{
    if (s.empty()) return -1;
    return max(s).index;
}

/// Return the min of v
template<typename T>
T min(const std::set<T>& v)
{
    T t = *v.begin();
    for(const auto& i: v)
	t = std::min(t,*i);

    return t;
}

// Return the list of dummy variable indices that are bound at the top level of the expression
std::set<dummy> get_bound_indices(const expression_ref& E)
{
    std::set<dummy> bound;

    if (not E.size()) return bound;

    // Make sure we don't try to substitute for lambda-quantified dummies
    if (E.head().type() == lambda_type)
    {
	if (E.sub()[0].is_a<dummy>())
	    bound.insert(E.sub()[0].as_<dummy>());
    }
    else 
    {
	if (E.head().type() == let_type)
	{
	    const int L = (E.size()-1)/2;
	    for(int i=0;i<L;i++)
	    {
		if (E.sub()[1+2*i].is_a<dummy>())
		    bound.insert(E.sub()[1+2*i].as_<dummy>());
	    }
	}
	assert(not E.head().is_a<Case>());
    }

    return bound;
}

void get_free_indices2(const expression_ref& E, multiset<dummy>& bound, set<dummy>& free)
{
    // fv x = { x }
    if (is_dummy(E))
    {
	dummy d = E.as_<dummy>();
	if (not is_wildcard(E) and (bound.find(d) == bound.end()))
	    free.insert(d);
	return;
    }

    // fv c = { }
    if (not E.size()) return;

    // for case expressions get_bound_indices doesn't work correctly.
    if (E.head().type() == case_type)
    {
	get_free_indices2(E.sub()[0], bound, free);

	const int L = (E.size()-1)/2;

	for(int i=0;i<L;i++)
	{
	    std::set<dummy> bound_ = get_free_indices(E.sub()[1+2*i]);
	    for(const auto& d: bound_)
		bound.insert(d);
	    get_free_indices2(E.sub()[2+2*i], bound, free);
	    for(const auto& d: bound_)
	    {
		auto it = bound.find(d);
		bound.erase(it);
	    }
	}

	return;
    }

    std::set<dummy> bound_ = get_bound_indices(E);
    for(const auto& d: bound_)
	bound.insert(d);
    for(int i=0;i<E.size();i++)
	get_free_indices2(E.sub()[i], bound, free);
    for(const auto& d: bound_)
    {
	auto it = bound.find(d);
	bound.erase(it);
    }
}

std::set<dummy> get_free_indices(const expression_ref& E)
{
    multiset<dummy> bound;
    set<dummy> free;
    get_free_indices2(E, bound, free);
    return free;
}

int get_safe_binder_index(const expression_ref& E)
{
    std::set<dummy> free = get_free_indices(E);
    if (free.empty()) 
	return 0;
    else
	return max_index(free)+1;
}

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

expression_ref Cons = lambda_expression( right_assoc_constructor(":",2) );

expression_ref ListEnd = lambda_expression( constructor("[]",0) );

vector<expression_ref> get_ref_vector_from_list(const expression_ref& E)
{
    vector<expression_ref> V;

    expression_ref E2 = E;
    while(has_constructor(E2,":"))
    {
	assert(E2.size() == 2);
	V.push_back(E2.sub()[0]);
	E2 = E2.sub()[1];
    }
    assert(has_constructor(E2,"[]"));

    return V;
}

template<> expression_ref get_tuple<>(const vector<expression_ref>& S)
{
    if (S.size() == 0) return constructor("()",0);

    if (S.size() == 1) return S[0];

    constructor H = tuple_head(S.size());

    if (not S.size()) return H;

    return new expression(H,S);
}

template<> expression_ref get_list<>(const vector<expression_ref>& v)
{
    expression_ref E = ListEnd;

    for(int i=v.size()-1;i>=0;i--)
	E = v[i]&E;

    return E;
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

template <typename T>
vector<T> skip(int n, const vector<T>& v)
{
    if (v.size() <= n) return vector<T>();

    vector<T> v2(v.size() - n);
    for(int i=0;i<v2.size();i++)
	v2[i] = v[i+n];

    return v2;
}

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
	assert(not E.head().is_a<lambda>());

	if (type == lambda2_type or type == constructor_type) 
	    return true;
	else
	    return false;
    }
    else
    {
	if (type == parameter_type) 
	    return false;
	else if (type == modifiable_type) 
	    return false;
	else
	    return true;
    }
}

bool is_dummy(const expression_ref& E)
{
    return (E.head().type() == dummy_type);
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

bool is_wildcard(const dummy& d)
{
    return d.is_wildcard();
}

// Remove in favor of is_dummy?
bool is_wildcard(const expression_ref& E)
{
    if (is_dummy(E))
    {
	assert(not E.size());
	dummy d = E.as_<dummy>();
	return is_wildcard(d);
    }
    else
	return false;
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
    if (E.head().is_a<let_obj>())
    {
	vector<expression_ref> vars;
	vector<expression_ref> bodies;
	expression_ref T;
	parse_let_expression(E, vars, bodies, T);

	// unnormalize T and the bodies
	T = launchbury_unnormalize(T);
	for(int i=0; i<vars.size(); i++)
	    bodies[i] = launchbury_unnormalize(bodies[i]);

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

	    for(int i=vars.size()-1; i>=0; i--)
	    {
		auto& V = vars[i].as_<dummy>();
		std::set<dummy> free = get_free_indices(bodies[i]);

		// if V references itself then don't substitute it.
		if (free.count(V)) continue;
	
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

const expression_ref v0 = dummy(0);
const expression_ref v1 = dummy(1);
const expression_ref v2 = dummy(2);
const expression_ref v3 = dummy(3);
const expression_ref v4 = dummy(4);
const expression_ref v5 = dummy(5);
const expression_ref v6 = dummy(6);
const expression_ref v7 = dummy(7);
const expression_ref v8 = dummy(8);

expression_ref char_list(const string& s)
{
    vector<expression_ref> letters;
    for(char c: s)
	letters.push_back(c);
    return get_list(letters);
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
