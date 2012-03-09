#include "expression.H"
#include "util.H"
#include "operation.H"
#include "operations.H"
#include <set>
#include <iterator>
#include <map>
#include "graph_register.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::set;

using boost::dynamic_pointer_cast;

//let [(x[i], bodies[i])] T
bool parse_let_expression(const expression_ref& R, vector<expression_ref>& vars, vector<expression_ref>& bodies, expression_ref& T)
{
  vars.clear();
  bodies.clear();

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return false;

  if (not dynamic_pointer_cast<let_obj>(E->sub[0])) return false;

  vector<expression_ref> pairs = get_ref_vector_from_list(E->sub[1]);
  for(int i=0;i<pairs.size();i++)
  {
    shared_ptr<const expression> E2 = dynamic_pointer_cast<const expression>(pairs[i]);
    assert(is_dummy(E2->sub[1]));
    vars.push_back(E2->sub[1]);
    bodies.push_back(E2->sub[2]);
  }

  T = E->sub[2];

  assert(vars.size() == bodies.size());

  return true;
}

void parse_alternatives(const expression_ref& R, vector<expression_ref>& cases, vector<expression_ref>& results)
{
  cases.clear();
  results.clear();

  vector<expression_ref> pairs = get_ref_vector_from_list(R);
  for(int i=0;i<pairs.size();i++)
  {
    shared_ptr<const expression> E2 = dynamic_pointer_cast<const expression>(pairs[i]);
    cases.push_back(E2->sub[1]);
    results.push_back(E2->sub[2]);
  }
}

//case T [(patterns[i],E[i])]
bool parse_case_expression(const expression_ref& R, expression_ref& T, vector<expression_ref>& vars, vector<expression_ref>& bodies)
{
  vars.clear();
  bodies.clear();

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return false;

  if (not dynamic_pointer_cast<Case>(E->sub[0])) return false;

  vector<expression_ref> pairs = get_ref_vector_from_list(E->sub[2]);
  for(int i=0;i<pairs.size();i++)
  {
    shared_ptr<const expression> E2 = dynamic_pointer_cast<const expression>(pairs[i]);
    vars.push_back(E2->sub[1]);
    bodies.push_back(E2->sub[2]);
  }

  T = E->sub[1];

  return true;
}


// How do I make constructor-specific methods of printing data expressions?
// Can I move to defining the print function using an expression?
string expression::print() const 
{
  string result;
  assert(sub[0]);

  //  if (false)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

    if (parse_let_expression(*this, vars, bodies, T))
    {
      result = "let {";
      vector<string> parts;
      for(int i=0;i<vars.size();i++)
	parts.push_back(vars[i]->print() + " = " + bodies[i]->print());
      result += join(parts,',');
      result += "} in " + T->print();
      return result;
    }

    if (parse_case_expression(*this, T, vars, bodies))
    {
      result = "case " + T->print() + " of {";
      vector<string> parts;
      for(int i=0;i<vars.size();i++)
	parts.push_back( vars[i]->print() + " -> " + bodies[i]->print() );
      result += join(parts,',');
      result += "}";
      return result;
    }
  }

  // Print the (unparenthesized) sub-expressions
  vector<string> args(size());
  for(int i=0;i<size();i++)
    args[i] = sub[i]->print();

  vector<string> pargs = args;
  for(int i=0;i<size();i++)
  {
    const expression* E = dynamic_cast<const expression*>(&*sub[i]);
    if (not E) continue;

    const Operator* O = dynamic_cast<const Operator*>(&*E->sub[0]);

    if (O and O->name() == "()") continue;

    pargs[i] = "(" + args[i] + ")";
  }
  
  if (const Operator* O = dynamic_cast<const Operator*>(&*sub[0]))
  {
    string O_name = O->name();
    if (dynamic_cast<const Apply*>(O))
      O_name = " ";

    if (O->precedence() > -1)
    {
      assert(O->n_args() == 2);
      if (const expression* E = dynamic_cast<const expression*>(&*sub[1]))
      {
	if (O->compare(*E->sub[0]) and O->associativity()==assoc_left)
	  pargs[1] = args[1];
	else if (const Operator* O2 = dynamic_cast<const Operator*>(&*E->sub[0]))
	  if (O2->precedence() > O->precedence())
	    pargs[1] = args[1];
      }
      if (const expression* E = dynamic_cast<const expression*>(&*sub[2]))
      {
	if (O->compare(*E->sub[0]) and O->associativity()==assoc_right)
	  pargs[2] = args[2];
	else if (const Operator* O2 = dynamic_cast<const Operator*>(&*E->sub[0]))
	  if (O2->precedence() > O->precedence())
	    pargs[2] = args[2];
      }
      return pargs[1] + O_name + pargs[2];
    }
    else if (O->name() == "()")
    {
      // Should Tuple's parenthesis sub-expressions?
      vector<string> sub_names;
      for(int i=1;i<size();i++)
	sub_names.push_back( args[i] );
      return "(" + join(sub_names,", ") + ")";
    }
      
    return O->print_expression( pargs );
  }

  // *this is an application expression
  if (const expression* E = dynamic_cast<const expression*>(&*sub[0]))
  {
    // this->sub[0] is also an application expression
    if (dynamic_cast<const expression*>(&*E->sub[0]) or is_dummy(E->sub[0]))
      // Don't parenthesize sub[0]
      pargs[0] = args[0];
  }

  return print_operator_expression( pargs );
}

tribool expression::compare(const Object& o) const 
{
  const expression* E = dynamic_cast<const expression*>(&o);
  if (not E) 
    return false;

  if (size() != E->size()) return false;

  tribool same = true;
  for(int i=0;i<size();i++) 
  {
    tribool b = sub[i]->compare(*E->sub[i]);

    if (indeterminate(b))
      std::cerr<<"Warning: '"<<sub[i]<<"' and '"<<E->sub[i]<<"' are unsure if they are equal.\n\n";

    same = same and b;
    if (not same) return false;
  }

  return same;
}

expression::expression(const expression_ref& E)
  :sub(1,E)
{}

expression::expression(const expression_ref& E1, const expression_ref& E2)
{
  sub.push_back(E1);
  sub.push_back(E2);
}

expression::expression(const std::vector< expression_ref >& E)
  :sub(E)
{ }

tribool constant::compare(const Object& o) const 
{
  const constant* E = dynamic_cast<const constant*>(&o);
  if (not E) 
    return false;

  return value->compare(*E->value);
}

tribool dummy::compare(const Object& o) const 
{
  const dummy* E = dynamic_cast<const dummy*>(&o);
  if (not E) 
    return false;

  if (name.size())
    return name == E->name;
  else
    return index == E->index;
}

string dummy::print() const {
  if (name.size())
    return name;
  else if (index < 0)
    return "_";
  else
    return string("#")+convertToString(index);
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

tribool match::compare(const Object& o) const 
{
  const match* E = dynamic_cast<const match*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

string match::print() const 
{
  if (index == -1) 
    return "_";
  else
    return string("_")+convertToString(index);
}

string let_obj::print() const 
{
  return "let";
}

tribool let_obj::compare(const Object& O) const
{
  if (this == &O) 
    return true;
  
  if (typeid(*this) != typeid(O)) return false;
  
  return true;
}

string alt_obj::name() const 
{
  return "->";
}

tribool alt_obj::compare(const Object& O) const
{
  if (this == &O) 
    return true;
  
  if (typeid(*this) != typeid(O)) return false;
  
  return true;
}

string equal_obj::name() const 
{
  return "=";
}

tribool equal_obj::compare(const Object& O) const
{
  if (this == &O) 
    return true;
  
  if (typeid(*this) != typeid(O)) return false;
  
  return true;
}

expression_ref Alt(const expression_ref& pattern, const expression_ref& body)
{
  // We can't just substitute into \x \y Alt(x,y) cuz alt_obj binds its first argument.
  expression* E = new expression(alt_obj() );
  E->sub.push_back(pattern);
  E->sub.push_back(body);
  return E;
}

// How would we handle lambda expressions, here?
bool find_match(const expression_ref& pattern, const expression_ref& E, vector< expression_ref >& results)
{
  // if this is a match expression, then succeed, and store E as the result of the match
  shared_ptr<const match> M = dynamic_pointer_cast<const match>(pattern);
  if (M) 
  {
    if (M->index >= 0)
    {
      if (results.size() < M->index+1) results.resize(M->index+1);

      if (results[M->index]) throw myexception()<<"Match expression contains match index "<<M->index<<"' more than once!";

      results[M->index] = E;
    }

    return true;
  }

  shared_ptr<const expression> pattern_exp = dynamic_pointer_cast<const expression>(pattern);

  // If this is a leaf constant, then check if E is equal to it.
  if (not pattern_exp)
    return (pattern->compare(*E) == true);

  // If pattern is an expression but E is not, then there is no match.
  shared_ptr<const expression> E_exp = dynamic_pointer_cast<const expression>(E);
  if (not E_exp) return false;

  // Expressions must have the same number of arguments
  if (pattern_exp->size() != E_exp->size()) return false;

  // Sub-expressions must match
  for(int i=0;i<pattern_exp->size();i++)
    if (not find_match(pattern_exp->sub[i], E_exp->sub[i], results))
      return false;

  return true;
}

tribool parameter::compare(const Object& o) const 
{
  const parameter* E = dynamic_cast<const parameter*>(&o);
  if (not E) 
    return false;

  return parameter_name == E->parameter_name;
}

tribool lambda::compare(const Object& O) const 
{
  if (this == &O) 
    return true;
  
  if (typeid(*this) != typeid(O)) return false;
  
  return true;
}

string lambda::print() const {
  return "lambda";
}

expression_ref lambda_quantify(int dummy_index, const expression_ref& R)
{
  expression* E = new expression(lambda());
  E->sub.push_back(dummy(dummy_index));
  E->sub.push_back(R);
  return E;
}

expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& R)
{
  expression* E = new expression(lambda());
  E->sub.push_back(dummy);
  E->sub.push_back(R);
  return E;
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

tribool constructor::compare(const Object& o) const
{
  const constructor* E = dynamic_cast<const constructor*>(&o);
  if (not E) 
    return false;

  return f_name == E->f_name;
}

constructor::constructor(const string& s, int n)
  :f_name(s), n_args_(n), assoc(assoc_none),prec(-1)
{
  
}

constructor left_assoc_constructor(const std::string& s,int prec)
{
  constructor f(s, 2);
  f.prec = prec;
  f.assoc = assoc_left;
  return f;
}

constructor right_assoc_constructor(const std::string& s,int prec)
{
  constructor f(s, 2);
  f.prec = prec;
  f.assoc = assoc_right;
  return f;
}

/// Do the substitutions match(i) -> replace[i] for all i where replace[i] is not null.
expression_ref substitute(const expression_ref& R, const vector<expression_ref>& replace)
{
  expression_ref R2 = R;
  for(int i=0;i<replace.size();i++)
    if (replace[i])
      R2 = substitute(R2, match(i), replace[i]);

  return R2;
}

expression_ref substitute(const expression_ref& R1, int dummy_index, const expression_ref& R2)
{
  return substitute(R1,dummy(dummy_index),R2);
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
/// 2. However, is sometimes still necessary to rename dummies.  This is true if R2 contains unbound dummies
///    that are bound in R1.
///
///    For example, apply Lx.y to x, then we would get Lx.x, which is not allowed.
///    Instead, we must "alpha-convert" Lx.y to Lz.y, and then apply Lz.y to x, leading to Lz.x .

/// Literally R2 for D in R1. (e.g. don't rename variables in R2).  Throw an exception if D is a lambda-bound dummy variable.

std::set<dummy> get_free_indices(const expression_ref& R);

// Return the list of dummy variable indices that are bound at the top level of the expression
std::set<dummy> get_bound_indices(const expression_ref& R)
{
  std::set<dummy> bound;

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return bound;

  // Make sure we don't try to substitute for lambda-quantified dummies
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]))
  {
    if (shared_ptr<const dummy> D  = dynamic_pointer_cast<const dummy>(E->sub[1]))
      bound.insert(*D);
  }
  else if (dynamic_pointer_cast<const alt_obj>(E->sub[0]))
    bound = get_free_indices(E->sub[1]);
  else 
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;
    if (parse_let_expression(R, vars, bodies, T))
    {
      // Don't substitute into local variables.
      for(int i=0;i<vars.size();i++)
	if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(vars[i]))
	  bound.insert(*D);
    }
  }

  return bound;
}

// Return the list of dummy variable indices that are bound at the top level of the expression
void alpha_rename(shared_ptr<expression>& E, const expression_ref& x, const expression_ref& y)
{
  // assert: x is bound in E
  // assert: y is neither bound in E nor free in E

  // std::cout<<" replacing "<<x<<" with "<<y<<" in "<<E->print()<<":\n";
  // Make sure we don't try to substitute for lambda-quantified dummies
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]))
  {
    assert(E->sub[1]->compare(*x));
    E->sub[1] = y;
    E->sub[2] = substitute(E->sub[2], x, y);
  }
  else if (dynamic_pointer_cast<const alt_obj>(E->sub[0]))
  {
    // assert(pattern E->sub[1] contains x
    E->sub[1] = substitute(E->sub[1], x, y);
    E->sub[2] = substitute(E->sub[2], x, y);
  }
  else if (dynamic_pointer_cast<const let_obj>(E->sub[0]))
  {
    E->sub[2] = substitute(E->sub[2], x, y);

    // This is kind of an awkward way to simultaneously walk/modify an expression
    expression_ref* R2 = &E->sub[1];
    bool found = false;
    while(shared_ptr<expression> E2 = dynamic_pointer_cast<expression>(*R2))
    {
      shared_ptr<expression> E3 = dynamic_pointer_cast<expression>(E2->sub[1]);
      assert(E3);

      // substitute in the body of v = expression
      E3->sub[2] = substitute(E3->sub[2], x, y);

      // substitute for the bound variable
      if (x->compare(*E3->sub[1]))
      {
	E3->sub[1] = y;
	found = true;
      }

      // Go to the next definition
      R2 = &E2->sub[2];
    }
    assert(found);
  }
  else
    assert(false);
  // std::cout<<"    "<<E->print()<<"\n";
}

std::set<dummy> get_free_indices(const expression_ref& R)
{
  std::set<dummy> S;

  // fv x = { x }
  if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R)) 
  {
    S.insert(*D);
    return S;
  }

  // fv c = { }
  shared_ptr< const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return S;

  std::set<dummy> bound = get_bound_indices(R);

  for(int i=0;i<E->size();i++)
    add(S, get_free_indices(E->sub[i]));

  foreach(i,bound)
    S.erase(*i);

  return S;
}

/// Return the min of v
template<typename T>
T max(const std::set<T>& v)
{
  T t = *v.begin();
  foreach(i,v)
    t = std::max(t,*i);

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
  foreach(i,v)
    t = std::min(t,*i);

  return t;
}

int get_safe_binder_index(const expression_ref& R)
{
  std::set<dummy> free = get_free_indices(R);
  if (free.empty()) 
    return 0;
  else
    return max_index(free)+1;
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

void do_substitute(expression_ref& R1, const expression_ref& D, const expression_ref& R2)
{
  assert(not is_wildcard(D));

  // If this is the relevant dummy, then substitute
  if (D->compare(*R1))
  {
    R1 = R2;
    return;
  }

  // FIXME: If we modify R1 later, will this modification show up in E1?
  shared_ptr<expression> E1 = dynamic_pointer_cast<expression>(R1);

  // If this is any other constant, then it doesn't contain the dummy
  if (not E1) return;

  // What indices are bound at the top level?
  std::set<dummy> bound = get_bound_indices(R1);

  if (not bound.empty())
  {
    // Don't substitute into local variables
    foreach(i,bound)
    {
      if (D->compare(dummy(*i))) return;
    }
    
    std::set<dummy> fv2 = get_free_indices(R2);
    std::set<dummy> overlap = intersection(bound,fv2);
    
    // If some of the free variables in R2 are bound in R1, then do alpha-renaming on R1 to avoid name capture.
    if (not overlap.empty())
    {
      // Determine the free variables of R1 so that we can avoid them in alpha renaming
      std::set<dummy> fv1 = get_free_indices(R1);

      // If R1 does not contain D, then we won't do any substitution anyway, so avoid alpha renaming.
      if (shared_ptr<const dummy> D2 = dynamic_pointer_cast<const dummy>(D))
      {
	if (fv1.find(*D2) == fv1.end()) return;
      }

      // Compute the total set of free variables to avoid clashes with when alpha renaming.
      add(fv2, get_free_indices(R1));

      // we don't want to rename on top of any other variables bound here
      int new_index = std::max(max_index(fv2),max_index(bound))+1;

      // Do the alpha renaming
      foreach(i,overlap)
	alpha_rename(E1, dummy(*i), dummy(new_index++));

      // We rename a bound variable dummy(*i) in R1 that is free in R2 to a new variable dummy(new_index)
      //   that is not bound or free in the initial version of R1 and free in R2.

      // The conditions are therefore:
      //   dummy(*i) must be bound in R1
      //   dummy(new_index) must be neither bound nor free in R1
      //   dummy(new_index) must not be free in R2
    }
  }

  // Since this is an expression, substitute into sub-expressions
  for(int i=0;i<E1->size();i++)
    do_substitute(E1->sub[i], D, R2);
}

bool find_let_statements_with_bound_vars(const vector<expression_ref>& let_vars, const vector<expression_ref>& let_bodies,
					 const set<dummy>& bound,
					 vector<int>& bound_indices, vector<int>& unbound_indices)
{
  set<dummy> let_bound;
  for(int i=0;i<let_vars.size();i++)
    let_bound.insert(*dynamic_pointer_cast<const dummy>(let_vars[i]));

  // Find the set of bound variables that could be free in let_bodies
  set<dummy> visible_bound = bound;
  foreach(i, let_bound)
    visible_bound.erase(*i);

  vector< set<dummy> > free_vars;
  for(int i=0;i<let_bodies.size();i++)
    free_vars.push_back( get_free_indices( let_bodies[i] ) );

  bound_indices.clear();
  unbound_indices.clear();
  for(int i=0;i<let_bodies.size();i++)
    unbound_indices.push_back(i);

  // Find the indices that are not bound (directly or indirectly) by the bound variables
  set<dummy> new_bound = visible_bound;
  while (not new_bound.empty())
  {
    set<dummy> new_bound_next;
    for(int i=unbound_indices.size()-1;i>=0;i--)
    {
      int index = unbound_indices[i];
      if (not intersection(free_vars[index], new_bound).empty())
      {
	new_bound_next.insert(*dynamic_pointer_cast<const dummy>(let_vars[index]));
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

/// Given let vars=bodies in (<binder bound> (let R_vars=R_bodies in T)), 
///  move some of the R_vars=R_bodies up to vars=bodies.
expression_ref move_lets(bool scope, const expression_ref R, 
			 vector<expression_ref>& vars, vector<expression_ref>& bodies,
			 const set<dummy>& bound, const set<dummy>& free)
{
  assert(R);
  assert(vars.size() == bodies.size());

  vector<expression_ref> R_vars;
  vector<expression_ref> R_bodies;
  expression_ref R2 = R;

  if (not parse_let_expression(R, R_vars, R_bodies, R2))
    R2 = R;

  // Find the set of variables to avoid renaming over.
  set<dummy> avoid = free;
  for(int i=0;i<vars.size();i++)
  {
    dummy D = *dynamic_pointer_cast<const dummy>(vars[i]);
    avoid.insert(D);
    add(avoid, get_free_indices(bodies[i]));
  }
  add(avoid, get_free_indices(R));
  add(avoid, bound);

  int new_index = max_index(avoid) + 1;
    

  // Determine which of the let-statements in R we can float.
  vector<int> unbound_indices;
  vector<int> bound_indices;
  if (find_let_statements_with_bound_vars(R_vars, R_bodies, bound, bound_indices, unbound_indices))
  {
    // Renaming shouldn't hit any of the other let-binder-variables in R
    for(int i=0;i<R_vars.size();i++)
    {
      dummy D = *dynamic_pointer_cast<const dummy>(R_vars[i]);
      avoid.insert(D);
    }
    
    // alpha-rename R
    new_index = max_index(avoid) + 1;
    
    
    shared_ptr<expression> RR ( dynamic_pointer_cast<const expression>(R)->clone() );
    
    for(int i=0;i<unbound_indices.size();i++)
    {
      int index = unbound_indices[i];
      dummy D = *dynamic_pointer_cast<const dummy>(R_vars[index]);
      if (includes(avoid, D))
	alpha_rename(RR, D, dummy(new_index++));
    }
  
    R_vars.clear();
    R_bodies.clear();
    parse_let_expression(shared_ptr<const expression>(RR), R_vars, R_bodies, R2);
    
    // Add the alpha-renamed versions of the unbound vars/bodies to the higher-level environment
    for(int i=0;i<unbound_indices.size();i++)
    {
      int index = unbound_indices[i];
#ifndef NDEBUG
      dummy rv = *dynamic_pointer_cast<const dummy>(R_vars[index]);
      
      for(int j=0;j<vars.size();j++)
	assert(not includes(avoid, rv));
      
      avoid.insert(rv);
#endif
      vars.push_back(R_vars[index]);
      bodies.push_back(R_bodies[index]);
    }

    // Construct the remainder expression
    vector<expression_ref> R_vars2;
    vector<expression_ref> R_bodies2;
    for(int i=0;i<bound_indices.size();i++)
    {
      int index = bound_indices[i];
      
      R_vars2.push_back(R_vars[index]);
      R_bodies2.push_back(R_bodies[index]);
    }

    R2 = let_expression(R_vars2, R_bodies2, R2);
  }
  // If nothing is moveable, then just return the original statement.
  else
    R2 = R;

  // We can't float this out because its bound, or because there's no bound to float it through.
  if ((not scope) or (not intersection(get_free_indices(R2), bound).empty()))
  {
    assert(R2);
    return R2;
  }

  // Since we only substitute reg_vars into dummy's (for let, lambda, and case) these are all OK.
  if (is_parameter(R2) or is_reg_var(R2) or is_var(R2) or is_dummy(R2))
  {
    assert(R2);
    return R2;
  }


  // If R2 is not bound, and its not a let-bound dummy, then create a new expression for it.
  dummy D2(new_index++);
  vars.push_back( D2 );
  bodies.push_back( R2 );
  return D2;
}

expression_ref move_lets(bool scope, const expression_ref R, 
			 vector<expression_ref>& vars, vector<expression_ref>& bodies,
			 const set<dummy>& bound)
{
  set<dummy> free;
  return move_lets(scope, R, vars, bodies, bound, free);
}

expression_ref move_lets(bool scope, const expression_ref R,
			 vector<expression_ref>& vars, vector<expression_ref>& bodies)
{
  set<dummy> bound;
  return move_lets(scope, R, vars, bodies, bound);
}

template <typename T>
bool operator==(const std::set<T>& S1, const std::set<T>& S2)
{
  return includes(S1,S2) and includes(S2,S2);
}

// When we let_float \x.\y.x, we should float out x, even though its a dummy

// However, if we have let {z=2} in \x.\y.z, we should not introduce a let dummy
// for z, because its already let bound.

expression_ref let_float(const expression_ref& R)
{
  // 0. NULL
  if (not R) return R;

  // 1. Dummy variable
  if (is_dummy(R))
    return R;
  
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  // 2. Literal constants.  Treat as 0-arg constructor.
  if (not E) return R;
  
  set<dummy> free_in_R = get_free_indices(R);

  vector<expression_ref> vars;
  vector<expression_ref> bodies;
  expression_ref T;
  expression_ref R2;

  // 3. Lambda expressions
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]))
  {
    // Find the new let-bound set.
    dummy D = *dynamic_pointer_cast<const dummy>(E->sub[1]);

    // First float lets in sub-expressions
    expression_ref M = let_float(E->sub[2]);

    // Determine the bound indices
    set<dummy> bound;
    bound.insert(D);

    // Move lets across the lambda
    M = move_lets(true, M, vars, bodies, bound, free_in_R);

    // Reassemble the expression
    R2 = let_expression(vars, bodies, lambda_quantify(D, M) );
  }

  // 4. Case expressions
  else if (parse_case_expression(R,T,vars,bodies))
  {
    vector<expression_ref> let_vars;
    vector<expression_ref> let_bodies;

    // First float out of case object (bound = {}, free = fv(R))
    T = let_float(T);
    T = move_lets(true, T, let_vars, let_bodies, set<dummy>(), free_in_R);

    for(int i=0;i<bodies.size();i++)
    {
      // Find the bound variables in the i-th constructor
      set<dummy> bound;
      if (shared_ptr<const expression> C = dynamic_pointer_cast<const expression>(vars[i]))
      {
	assert(dynamic_pointer_cast<const constructor>(C->sub[0]));
	for(int j=1;j<C->size();j++)
	{
	  dummy D = *dynamic_pointer_cast<const dummy>(C->sub[j]);
	  if (not is_wildcard(D))
	    bound.insert(D);
	}
      }

      // First float out of case object (bound = {}, free = fv(R))
      bodies[i] = let_float(bodies[i]);
      bodies[i] = move_lets(true, bodies[i], let_vars, let_bodies, bound, free_in_R);
    }

    R2 = let_expression(let_vars, let_bodies, case_expression(false, T, vars, bodies));
  }

  // 5. Let expressions
  else if (parse_let_expression(R,vars,bodies,T))
  {
    // Return let_float(T) if T doesn't mention any of the newly let-bound variables
    set<dummy> bound_vars_let;
    for(int i=0;i<vars.size();i++)
      bound_vars_let.insert(*dynamic_pointer_cast<const dummy>(vars[i]));

    set<dummy> free_vars_T = get_free_indices(T);
    if (intersection(bound_vars_let, free_vars_T).empty()) 
      return let_float(T);

    // First float lets in sub-expressions
    T = let_float(T);
    for(int i=0;i<bodies.size();i++)
      bodies[i] = let_float(bodies[i]);

    // Move lets out of T and into vars
    T = move_lets(false, T, vars, bodies, set<dummy>(), free_in_R);

    // Move lets out of bodies and into vars
    for(int i=0;i<bodies.size();i++)
      bodies[i] = move_lets(false, bodies[i], vars, bodies, set<dummy>(), free_in_R);

    R2 = let_expression(vars,bodies,T);
  }

  // 6. Handle application, constructors, and operations.
  else if (shared_ptr<const Operator> O =  dynamic_pointer_cast<const Operator>(E->sub[0]))
  {
    // First float lets in sub-expressions
    shared_ptr<expression> V ( E->clone() );
    
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    
    // Move lets from arguments into (vars,bodies)
    for(int i=1;i<E->size();i++)
    {
      V->sub[i] = let_float(V->sub[i]);
      V->sub[i] = move_lets(true, V->sub[i], vars, bodies, set<dummy>(), free_in_R);
    }
      
    R2 = let_expression(vars, bodies, shared_ptr<const expression>(V));
  }
  else
    throw myexception()<<"let_float: I don't understand expression '"<<R<<"'";

#ifndef NDEBUG
  set<dummy> S2 = get_free_indices(R2);
  assert(free_in_R == S2);
#endif

  return R2;
}

expression_ref substitute(const expression_ref& R1, const expression_ref& D, const expression_ref& R2)
{
  expression_ref R1b = R1;
  do_substitute(R1b, D, R2);
  return R1b;
}


expression_ref apply_expression(const expression_ref& R,const expression_ref& arg)
{
  expression* E = new expression(Apply());
  E->sub.push_back(R);
  E->sub.push_back(arg);
  return E;
}

expression_ref apply_expression(const expression_ref& E,
				const vector< expression_ref > args)
{
  expression_ref E2 = E;
  for(int i=0;i<args.size();i++)
    E2 = apply_expression(E2,args[i]);
  return E2;
}


// When applying Lx.M to N, we need to make sure that no occurrence of x has the free variables in N bound.
// At each occurence of x, we need to know 
// (i) what are the lambda's that class with the free variables of N
// (ii) what free variables of M are 
expression_ref apply(const expression_ref& R,const expression_ref& arg)
{
  assert(R);

  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]))
      return substitute(E->sub[2], E->sub[1], arg);
  }

  // Allow applying non-lambda expressions to arguments.
  // We need this to apply variables that turn out to be functions.
  return apply_expression(R,arg);
}

expression_ref apply(const expression_ref& E,
		     const vector< expression_ref > args)
{
  expression_ref E2 = E;
  for(int i=0;i<args.size();i++)
    E2 = apply(E2,args[i]);
  return E2;
}

expression_ref operator,(const expression_ref& E1, const expression_ref& E2)
{
  return apply(E1, E2);
}

expression_ref operator&(const expression_ref& E1, const expression_ref& E2)
{
  return (Cons,E1,E2);
}

void find_named_parameters(const expression_ref& R, std::set<string>& names)
{
  assert(R);
  // If this is a parameter, then makes sure we've got its name.
  if (shared_ptr<const parameter> n = dynamic_pointer_cast<const parameter>(R))
  {
    if (names.find(n->parameter_name) == names.end())
      names.insert(n->parameter_name);
  }

  // If this is an expression, check its sub-objects
  else if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      find_named_parameters(E->sub[i], names);
  }
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

expression_ref add_prefix(const string& prefix, const expression_ref& R)
{
  std::set<string> names = find_named_parameters(R);

  expression_ref R2 = R;
  foreach(i,names)
    R2 = substitute(R2, parameter(*i), parameter(prefix+"::"+*i));

  return R2;
}

expression_ref Tuple(int n)
{
  return lambda_expression( constructor("()",n) );
}

expression_ref Tuple(const expression_ref& R1,const expression_ref& R2)
{
  return Tuple(2)(R1)(R2);
}

expression_ref Tuple(const expression_ref& R1,const expression_ref& R2,const expression_ref& R3)
{
  return Tuple(3)(R1)(R2)(R3);
}

expression_ref Tuple(const expression_ref& R1,const expression_ref& R2,const expression_ref& R3,const expression_ref& R4)
{
  return Tuple(4)(R1)(R2)(R3)(R4);
}

expression_ref Tuple(const expression_ref& R1,const expression_ref& R2,const expression_ref& R3,const expression_ref& R4,const expression_ref& R5)
{
  return Tuple(5)(R1)(R2)(R3)(R4)(R5);
}


expression_ref Cons = lambda_expression( right_assoc_constructor(":",2) );

expression_ref ListEnd = lambda_expression( constructor("[]",0) );

expression_ref _ = match(-1);
expression_ref _1 = match(0);
expression_ref _2 = match(1);
expression_ref _3 = match(2);
expression_ref _4 = match(3);

expression_ref default_value = lambda_expression(constructor("default_value",2));

expression_ref var_bounds = lambda_expression(constructor("var_bounds",2));

// Fields: (prob_density) (random vars) (parameter expressions)
expression_ref distributed = lambda_expression( constructor("~",2) );

expression_ref sys_print = lambda_expression( Print() );

expression_ref concat = lambda_expression( Concat() );

expression_ref prob = lambda_expression( constructor("probability",1) );

expression_ref defun = lambda_expression( constructor("defun",3) );

vector<expression_ref> get_ref_vector_from_list(const expression_ref& R)
{
  expression_ref R2 = R;
  vector<expression_ref> V;
  while(boost::shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R2))
  {
    assert(E->size() == 3);
    V.push_back(E->sub[1]);
    R2 = E->sub[2];
  }

  return V;
}

std::vector<expression_ref> get_ref_vector_from_tuple(const expression_ref& R)
{
  boost::shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  if (not E)
    return std::vector<expression_ref>(1,R);
    
  std::vector<expression_ref> v2(E->size()-1);
  for(int i=0;i<v2.size();i++)
    v2[i] = E->sub[i+1];
  return v2;
}

template<> expression_ref get_tuple<>(const vector<expression_ref>& v)
{
  if (not v.size()) return Tuple(0);

  if (v.size() == 1) return v[0];

  vector<expression_ref> sub(v.size()+1);
  sub[0] = constructor("()",v.size());
  for(int i=0;i<v.size();i++)
    sub[i+1] = v[i];

  return expression_ref(expression(sub));
}

expression_ref get_list(const vector<expression_ref>& v)
{
  expression_ref E = ListEnd;

  for(int i=v.size()-1;i>=0;i--)
    E = Cons(v[i],E);

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

// FIXME: add operator expressions :-P

expression_ref let_expression(const vector<expression_ref>& vars, const vector<expression_ref>& bodies, const expression_ref& T)
{
  if (vars.size() == 0) return T;

  // merge with existing let expression...
  {
    vector<expression_ref> vars2;
    vector<expression_ref> bodies2;
    expression_ref T2;
    if (parse_let_expression(T, vars2, bodies2, T2))
    {
      vector<expression_ref> vars12 = vars;
      vector<expression_ref> bodies12 = bodies;
      vars12.insert(vars12.end(),vars2.begin(), vars2.end());
      bodies12.insert(bodies12.end(), bodies2.begin(), bodies2.end());
      return let_expression(vars12, bodies12, T2);
    }
  }

  expression* E = new expression( let_obj() );
  E->sub.push_back(ListEnd);
  E->sub.push_back(T);

  for(int i=0;i<vars.size();i++)
  {
    expression_ref t = lambda_expression( equal_obj() )(vars[i], bodies[i]);
    E->sub[1] = Cons(t, E->sub[1]);
  }

  return E;
}

expression_ref let_expression(const expression_ref& var, const expression_ref& body, const expression_ref& T)
{
  vector<expression_ref> vars(1,var);
  vector<expression_ref> bodies(1,body);
  return let_expression(vars, bodies, T);
}

bool is_irrefutable_pattern(const expression_ref& R)
{
  return dynamic_pointer_cast<const dummy>(R);
}

bool is_simple_pattern(const expression_ref& R)
{
  if (is_irrefutable_pattern(R)) return true;

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>( R );

  //  0-arg constructor, since we've already bailed on dummy variables
  if (not E) return true;

  assert(dynamic_pointer_cast<const constructor>(E->sub[0]));

  // Arguments of multi-arg constructors must all be irrefutable patterns
  for(int j=1;j<E->size();j++)
    if (not is_irrefutable_pattern(E->sub[j]))
      return false;

  return true;
}

// This function currently assumes that all the patterns are just variables.

expression_ref case_expression(bool decompose, const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies);

expression_ref simple_case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  expression_ref R = case_expression(false, T, patterns, bodies);

  for(int i=patterns.size()-1;i>=0;i--)
  {
    if (not is_simple_pattern(patterns[i]))
      throw myexception()<<"simple_case_expression( ): pattern '"<<patterns[i]<<"' is not free variable in expression '"<<R<<"'";
  }

  return R;
}

template <typename T>
vector<T> skip(int n, const vector<T>& v)
{
  if (v.size() <= n) return vector<T>();

  vector<T> v2(v.size() - n);
  for(int i=0;i<v2.size();i++)
    v2[i] = v2[i+n];

  return v2;
}

expression_ref case_expression(bool decompose, const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  using std::max;

  if (not decompose)
  {
    expression* E = new expression( Case() );
    E->sub.push_back(T);
    E->sub.push_back(ListEnd);

    for(int i=patterns.size()-1;i>=0;i--)
      E->sub[2] = Cons(Alt(patterns[i],bodies[i]), E->sub[2]);
    return E;
  }

  vector<expression_ref> ok_patterns;
  vector<expression_ref> ok_bodies;
  for(int i=0;i<patterns.size();i++)
  {
    ok_patterns.push_back(patterns[i]);
    ok_bodies.push_back(bodies[i]);
    int var_index = max(get_safe_binder_index(T), max(get_safe_binder_index(patterns[i]), get_safe_binder_index(bodies[i])));

    // 1. we don't have to decompose this if its an irrefutable pattern
    if (is_irrefutable_pattern((patterns[i]))) continue;

    shared_ptr<expression> PE = dynamic_pointer_cast<expression>( ok_patterns.back() );

    // 2. we don't have to decompose this if its a simple branch: 0-arg constructor
    if (not PE) continue;

    assert(dynamic_pointer_cast<const constructor>(PE->sub[0]));
    vector<int> complex_patterns;
    for(int j=1;j<PE->size();j++)
      if (not is_irrefutable_pattern(PE->sub[j]))
	complex_patterns.push_back(j);

    // 2. we don't have to decompose this if its a simple branch: n-arg constructor with all variable arguments.
    if (complex_patterns.empty()) continue;


    // 3a. Construct the expression to match if this pattern doesn't match.
    expression_ref otherwise;
    if (i < patterns.size()-1)
      otherwise = case_expression(true, T, skip(i+1, patterns), skip(i+1,bodies));
    
    // 3b. Construct the simple case expression and modified body for this expression.
    vector<expression_ref> sub_terms;
    vector<expression_ref> sub_patterns;
    for(int j=0;j<complex_patterns.size();j++)
    {
      int index = complex_patterns[j];

      // y ~ PE->sub[index]
      expression_ref new_var = dummy(var_index++);
      sub_terms.push_back(new_var);
      sub_patterns.push_back(PE->sub[index]);
      PE->sub[index] = new_var;
    }

    ok_bodies.back() = multi_case_expression(true, sub_terms, sub_patterns, ok_bodies.back(), otherwise);

    return simple_case_expression(T, ok_patterns, ok_bodies);
  }

  return simple_case_expression(T, patterns, bodies);
}

expression_ref case_expression(bool decompose, const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise)
{
  vector<expression_ref> patterns(1, pattern);
  vector<expression_ref> bodies(1, body);
  if (otherwise and not dynamic_pointer_cast<const dummy>(pattern))
  {
    patterns.push_back(dummy(-1));
    bodies.push_back(otherwise);
  }
  return case_expression(decompose, T,patterns, bodies);
}

/*
 * \todo FIXME
 * Can we avoid repeating the same sub-expression on several else-branches of a multi-case expression
 * by introducing sharing with a let construct?
 */

expression_ref multi_case_expression(bool decompose, const vector<expression_ref>& terms, const vector<expression_ref>& patterns, 
				     const expression_ref& body, const expression_ref& otherwise)
{
  assert(terms.size() == patterns.size());
  std::set<dummy> free;
  for(int i=0;i<terms.size();i++)
    add(free, get_free_indices(terms[i]));

  std::set<dummy> free_patterns;
  for(int i=0;i<patterns.size();i++)
    add(free_patterns, get_free_indices(patterns[i]));

  assert(intersection(free, free_patterns).empty());
  std::set<dummy> free_body = get_free_indices(body);
  remove(free_body, free_patterns);

  expression_ref R = body;
  for(int i=patterns.size()-1; i>=0; i--)
    R = case_expression(decompose, terms[i],patterns[i],R,otherwise);

  return R;
}

expression_ref def_function(bool decompose, const vector< vector<expression_ref> >& patterns, const vector<expression_ref>& bodies, const expression_ref& otherwise)
{
  // Find the first safe var index
  std::set<dummy> free;
  if (otherwise)
    free = get_free_indices(otherwise);

  for(int i=0;i<patterns.size();i++)
  {
    add(free, get_free_indices(bodies[i]));
  
    for(int j=0; j<patterns[i].size(); j++)
      add(free, get_free_indices(patterns[i][j]));
  }
  
  int var_index = 0;
  if (not free.empty()) var_index = max_index(free)+1;

  // All versions of the function must have the same arity
  assert(patterns.size());
  for(int i=1;i<patterns.size();i++)
    assert(patterns[0].size() == patterns[i].size());

  // Construct the dummies
  vector<expression_ref> args;
  for(int i=0;i<patterns[0].size();i++)
    args.push_back(dummy(var_index+i));
    
  // Construct the case expression
  expression_ref R = otherwise;
  for(int i=patterns.size()-1; i>=0; i--)
  {
    vector<expression_ref> test_args;
    vector<expression_ref> test_patterns;
    expression_ref body = bodies[i];

    for(int j=0;j<patterns[i].size();j++)
    {
      const expression_ref& P = patterns[i][j];

      // If the pattern is irrefutable, then just substitute in the corresponding arg[]
      if (is_irrefutable_pattern(P))
      {
	if (not is_wildcard(P))
	  body = substitute(body, P, args[j]);
      }
      // If the pattern involves a test, record the test.
      else
      {
	test_args.push_back(args[j]);
	test_patterns.push_back(P);
      }
    }

    // Only add tests if there are any tests.
    if (test_args.size())
      R = multi_case_expression(decompose, test_args, test_patterns, body, R);

    // If there are no tests, then the 'otherwise' condition cannot occur.  This should only happen on the last pattern.
    else
    {
      R = body;
      assert(i==patterns.size()-1);
    }
  }

  assert(R);

  // Turn it into a function
  for(int i=patterns[0].size()-1;i>=0;i--)
    R = lambda_quantify(var_index+i, R);

  return R;
}

expression_ref def_function(bool decompose, const vector<expression_ref>& patterns, const expression_ref& body, const expression_ref& otherwise)
{
  return def_function(decompose, vector< vector<expression_ref> >(1,patterns), vector<expression_ref>(1,body), otherwise);
}

expression_ref def_function(bool decompose, const vector<expression_ref>& pattern, const vector<expression_ref>& bodies, const expression_ref& otherwise)
{
  vector< vector<expression_ref> > patterns;

  for(int i=0;i<pattern.size();i++)
  {
    patterns.push_back( vector<expression_ref>() );

    shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(pattern[i]);
    if (not E)
      patterns.back().push_back(pattern[i]);
    else
      for(int i=1;i<E->size();i++)
	patterns.back().push_back(E->sub[i]);
  }

  return def_function(decompose, patterns, bodies, otherwise);
}

expression_ref def_function(bool decompose, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise)
{
  return def_function(decompose, vector<expression_ref>(1,pattern), vector<expression_ref>(1,body), otherwise);
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

bool is_WHNF(const expression_ref& R)
{
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  if (E)
  {
    // 3. An Operation whose arguments cannot be evaluated, or that does not have all its arguments?
    // Neither case is allowed, currently.

    // 4. Lambda
    shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]);
    if (L) return true;

    // 5. Constructor
    shared_ptr<const constructor> RF = dynamic_pointer_cast<const constructor>(E->sub[0]);
    if (RF) return true;

    return false;
  }
  else
  {
    if (shared_ptr<const parameter> p = dynamic_pointer_cast<const parameter>(R))
      return false;

    // 1. a (dummy) variable.
    // 2. Literal constant.  Treat as 0-arg constructor.
    return true;
  }
}

bool is_dummy(const expression_ref& R)
{
  if (dynamic_cast<const dummy*>(&*R)) return true;

  return false;
}

bool is_parameter(const expression_ref& R)
{
  if (dynamic_cast<const parameter*>(&*R)) return true;

  return false;
}

bool is_wildcard(const expression_ref& R)
{
  shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R);
  if (not D) return false;
  if (D->name.size()) return false;

  return (D->index < 0);
}

expression_ref launchbury_normalize(const expression_ref& R)
{
  // 1. Var
  if (is_dummy(R))
    return R;
  
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E) return R;
  
  // 2. Lambda
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]);
  if (L)
  {
    assert(E->size() == 3);
    expression* V = new expression(*E);
    V->sub[2] = launchbury_normalize(E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return V;
  }

  // 3. Application
  if (dynamic_pointer_cast<const Apply>(E->sub[0]))
  {
    assert(E->size() == 3);
    if (is_dummy(E->sub[2]))
    { 
      expression* V = new expression(*E);
      V->sub[1] = launchbury_normalize(E->sub[1]);
      return V;
    }
    else
    {
      int var_index = get_safe_binder_index(R);
      expression_ref x = dummy(var_index);

      return let_expression(x, launchbury_normalize(E->sub[2]), apply_expression(launchbury_normalize(E->sub[1]),x));
    }
  }

  // 6. Case
  shared_ptr<const Case> IsCase = dynamic_pointer_cast<const Case>(E->sub[0]);
  if (IsCase)
  {
    expression* V = new expression(*E);

    V->sub[1] = launchbury_normalize(V->sub[1]);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[2]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> alternative = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(alternative);
      alternative->sub[2] = launchbury_normalize(alternative->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    return V;
  }

  // FIXME! Handle operations.
  // Extend the stack handling to be able to work on more than one argument.
  // Currently there is no need to evaluate arguments before applying them to functions.
  // Can we avoid evaluating functions before calling an operation?
  // - well, we could make the operation throw an exception identifying which is the first argument that needs to be
  //   evaluated.
  // - then, we could put the operation on the stack and begin evaluating just that one argument.
  
  // 4. Constructor
  if (dynamic_pointer_cast<const constructor>(E->sub[0]) or 
      dynamic_pointer_cast<const Operation>(E->sub[0]))
  {
    int var_index = get_safe_binder_index(R);

    expression* C = new expression;
    C->sub.push_back(E->sub[0]);

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=1;i<E->size();i++)
    {
      if (is_dummy(E->sub[i]))
      {
	C->sub.push_back(E->sub[i]);
      }
      else
      {
	expression_ref var = dummy( var_index++ );
	C->sub.push_back( var );
	vars.push_back( var );
	bodies.push_back( launchbury_normalize(E->sub[i]) );
      }
    }

    return let_expression(vars, bodies, C);
  }

  // 5. Let 
  shared_ptr<const let_obj> Let = dynamic_pointer_cast<const let_obj>(E->sub[0]);
  if (Let)
  {
    expression* V = new expression(*E);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[1]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> let_group = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(let_group);
      let_group->sub[2] = launchbury_normalize(let_group->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    V->sub[2] = launchbury_normalize(V->sub[2]);

    return V;
  }

  std::cerr<<"I don't recognize expression '"+ R->print() + "'\n";
  return R;
}

expression_ref launchbury_unnormalize(const expression_ref& R)
{
  // 1. Var
  if (is_dummy(R))
    return R;
  
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E) return R;
  
  // 2. Lambda
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]);
  if (L)
  {
    assert(E->size() == 3);
    expression* V = new expression(*E);
    V->sub[2] = launchbury_unnormalize(E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return V;
  }

  // 3. Application
  if (dynamic_pointer_cast<const expression>(E->sub[0]) or is_dummy(E->sub[0]))
  {
    expression* V = new expression(*E);
    V->sub[0] = launchbury_unnormalize(E->sub[0]);
    V->sub[1] = launchbury_unnormalize(E->sub[1]);
    return V;
  }
  
  // 6. Case
  shared_ptr<const Case> IsCase = dynamic_pointer_cast<const Case>(E->sub[0]);
  if (IsCase)
  {
    expression* V = new expression(*E);

    V->sub[1] = launchbury_unnormalize(V->sub[1]);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[2]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> alternative = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(alternative);
      alternative->sub[2] = launchbury_unnormalize(alternative->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    return V;
  }

  // 4. Constructor
  if (dynamic_pointer_cast<const constructor>(E->sub[0]) or 
      dynamic_pointer_cast<const Operation>(E->sub[0]))
  {
    expression* V = new expression(*E);
    for(int i=0;i<E->size();i++)
      V->sub[i] = launchbury_unnormalize(E->sub[i]);
    return V;
  }

  // 5. Let 
  shared_ptr<const let_obj> Let = dynamic_pointer_cast<const let_obj>(E->sub[0]);
  if (Let)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;
    parse_let_expression(R, vars, bodies, T);

    // unnormalize T and the bodies
    T = launchbury_unnormalize(T);
    for(int i=0; i<vars.size(); i++)
      bodies[i] = launchbury_unnormalize(bodies[i]);

    /*
      Identify cycles of size > 1...
      But what is the optimal behavior in that case?
    ublas::matrix<int> U(vars.size(), vars.size());
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
	shared_ptr<const dummy> V = dynamic_pointer_cast<const dummy>(vars[i]);
	assert(V);
	std::set<dummy> free = get_free_indices(bodies[i]);
	if (free.find(*V) != free.end()) continue;
	
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

  std::cerr<<"I don't recognize expression '"+ R->print() + "'\n";
  return R;

}
