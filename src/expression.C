#include "expression.H"
#include "util.H"
#include "operation.H"
#include "formula.H"
#include "context.H"
#include "operations.H"
#include <set>
#include <iterator>

using boost::shared_ptr;
using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

//let [(x[i], bodies[i])] T
bool parse_let_expression(const expression_ref& R, vector<expression_ref>& vars, vector<expression_ref>& bodies, expression_ref& T)
{
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return false;

  if (not dynamic_pointer_cast<let_obj>(E->sub[0])) return false;

  vector<expression_ref> pairs = get_ref_vector_from_list(E->sub[1]);
  for(int i=0;i<pairs.size();i++)
  {
    shared_ptr<const expression> E2 = dynamic_pointer_cast<const expression>(pairs[i]);
    vars.push_back(E2->sub[1]);
    bodies.push_back(E2->sub[2]);
  }

  T = E->sub[2];

  return true;
}

//case T [(patterns[i],E[i])]
bool parse_case_expression(const expression_ref& R, expression_ref& T, vector<expression_ref>& vars, vector<expression_ref>& bodies)
{
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return false;

  if (not dynamic_pointer_cast<case_obj>(E->sub[0])) return false;

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


vector<string> print_arg_expressions(const expression& e)
{
  vector<string> sub_names;
  for(int i=0;i<e.size();i++)
    sub_names.push_back( e.sub[i]->print() );
  
  return sub_names;
}

// How do I make constructor-specific methods of printing data expressions?
// Can I move to defining the print function using an expression?
string expression::print() const 
{
  string result;
  assert(sub[0]);

  if (const Operator* O = dynamic_cast<const Operator*>(&*sub[0]))
  {
    if (O->precedence() > -1)
    {
      assert(O->n_args() == 2);
      return sub[1]->print() + O->name() + sub[2]->print();
    }
    else if (O->name() == "Tuple")
    {
      vector<string> sub_names;
      for(int i=1;i<size();i++)
	sub_names.push_back( sub[i]->print() );
      return print_operator_expression(sub_names);
    }
      
    return O->print_expression(print_arg_expressions(*this));
  }

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
      result = "case " + T->print() + " in {";
      vector<string> parts;
      for(int i=0;i<vars.size();i++)
	parts.push_back( vars[i]->print() + " -> " + bodies[i]->print() );
      result += join(parts,',');
      result += "}";
      return result;
    }
  }

  return print_operator_expression( print_arg_expressions(*this) );
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

expression_ref::expression_ref(const term_ref& t)
  :polymorphic_cow_ptr<Object>((*t.F)[t.index])
{}

tribool dummy::compare(const Object& o) const {
  const dummy* E = dynamic_cast<const dummy*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

string dummy::print() const {
  if (index < 0)
    return "_";
  else
    return string("#")+convertToString(index);
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

string case_obj::print() const 
{
  return "case";
}

tribool case_obj::compare(const Object& O) const
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

tribool lambda::compare(const Object& o) const 
{
  const lambda* L = dynamic_cast<const lambda*>(&o);
  if (not L) 
    return false;

  return dummy_index == L->dummy_index;
}

string lambda::print() const {
  return string("lambda[")+convertToString(dummy_index)+"]";
}

lambda::lambda(int d)
  :dummy_index(d)
{ }

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
    R = expression_ref(expression(lambda(i),R));
  
  return R;
}

tribool Function::compare(const Object& o) const
{
  const Function* E = dynamic_cast<const Function*>(&o);
  if (not E) 
    return false;

  return f_name == E->f_name;
}

Function::Function(const string& s, int n, function_type_t f_t)
  :f_name(s), n_args_(n), what_type(f_t),assoc(assoc_none),prec(-1)
{
  
}

Function data_function(const std::string& s, int n)
{
  return Function(s, n, data_function_f);
}

Function left_assoc_data_function(const std::string& s,int prec)
{
  Function f(s, 2, data_function_f);
  f.prec = prec;
  f.assoc = assoc_left;
  return f;
}

Function right_assoc_data_function(const std::string& s,int prec)
{
  Function f(s, 2, data_function_f);
  f.prec = prec;
  f.assoc = assoc_right;
  return f;
}

Function body_function(const std::string& s, int n)
{
  return Function(s, n, body_function_f);
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

template <typename T>
void add(std::set<T>& S1, const std::set<T>& S2)
{
  std::set<T> result;
  std::merge(S1.begin(), S1.end(),
	     S2.begin(), S2.end(),
	     std::inserter(result, result.begin())
	);
  S1.swap(result);
}

template <typename T>
void remove(std::set<T>& S1, const std::set<T>& S2)
{
  std::set<T> result;
  std::set_difference(S1.begin(), S1.end(),
	     S2.begin(), S2.end(),
	     std::inserter(result, result.begin())
	);
  S1.swap(result);
}

template <typename T>
std::set<T> intersection(std::set<T>& S1, const std::set<T>& S2)
{
  std::set<T> result;
  std::set_intersection(S1.begin(), S1.end(),
			S2.begin(), S2.end(),
			std::inserter(result, result.begin())
	);
  return result;
}

std::set<int> get_free_indices(const expression_ref& R);

std::set<int> get_pattern_indices(const expression_ref& R)
{
  return get_free_indices(R);
}

// Return the list of dummy variable indices that are bound at the top level of the expression
std::set<int> get_bound_indices(const expression_ref& R)
{
  std::set<int> bound;

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return bound;

  // Make sure we don't try to substitute for lambda-quantified dummies
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]))
    bound.insert(L->dummy_index);
  else if (dynamic_pointer_cast<const alt_obj>(E->sub[0]))
    bound = get_pattern_indices(E->sub[1]);
  else 
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;
    if (parse_let_expression(R, vars, bodies, T))
    {
      // Don't substitute into local variables.
      for(int i=0;i<vars.size();i++)
      {
	shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(vars[i]);
	bound.insert(D->index);
      }
    }
  }

  return bound;
}

std::set<int> get_free_indices(const expression_ref& R)
{
  std::set<int> S;

  // fv x = { x }
  if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R)) 
  {
    S.insert(D->index);
    return S;
  }

  // fv c = { }
  shared_ptr< const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return S;

  std::set<int> bound = get_bound_indices(R);

  for(int i=0;i<E->size();i++)
    add(S, get_free_indices(E->sub[i]));

  foreach(i,bound)
    S.erase(*i);

  return S;
}

// Rename dummies even if they're bound.
static void rename_lambda(expression_ref& R, int old_name, int new_name)
{
  if (shared_ptr<dummy> D = dynamic_pointer_cast<dummy>(R))
  {
    if (D->index == old_name) D->index = new_name;
    return;
  }

  shared_ptr<expression> E = dynamic_pointer_cast<expression>(R);
  if (not E) return;

  if (shared_ptr<lambda> L = dynamic_pointer_cast<lambda>(E->sub[0]))
    if (L->dummy_index == old_name)
      L->dummy_index = new_name;

  // This is an expression, so compute the substituted sub-expressions
  for(int i=0;i<E->size();i++)
    rename_lambda(E->sub[i], old_name, new_name);
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

/// Return the min of v
template<typename T>
T min(const std::set<T>& v)
{
  T t = *v.begin();
  foreach(i,v)
    t = std::min(t,*i);

  return t;
}

static int get_safe_binder_index(const expression_ref& R)
{
  std::set<int> free = get_free_indices(R);
  if (free.empty()) 
    return 0;
  else
    return max(free)+1;
}

static int get_safe_binder_index_for_alt(const expression_ref& R1, const expression_ref& R2)
{
  std::set<int> pattern_vars = get_pattern_indices(R1);
  std::set<int> free = get_free_indices(R2);
  remove(free, pattern_vars);

  if (free.empty()) 
    return 0;
  else
    return max(free)+1;
}

// If we use de Bruijn indices, then, as before bound indices in R2 are no problem.
// Unlike before, we have a separate type for free variables: they therefore cannot be bound
//  by substituting them.

// Idea: replace lambda[index] with just lambda.
//  + Bound indices have in index that is the number of lambda terms 
//  + Binders can include let, lambda, and case.


void do_substitute(expression_ref& R1, const expression_ref& D, const expression_ref& R2)
{
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
  std::set<int> bound = get_bound_indices(R1);

  if (not bound.empty())
  {
    // Don't substitute into local variables
    foreach(i,bound)
    {
      if (D->compare(dummy(*i))) return;
    }
    
    std::set<int> fv2 = get_free_indices(R2);
    std::set<int> overlap = intersection(bound,fv2);
    
    // If some of the free variables in R2 are bound in R1, then do alpha-renaming on R1 to avoid name capture.
    if (not overlap.empty())
    {
      // Determine the free variables of R1 so that we can avoid them in alpha renaming
      std::set<int> fv1 = get_free_indices(R1);

      // If R1 does not contain D, then we won't do any substitution anyway, so avoid alpha renaming.
      if (shared_ptr<const dummy> D2 = dynamic_pointer_cast<const dummy>(D))
      {
	if (fv1.find(D2->index) == fv1.end()) return;
      }

      // Compute the total set of free variables to avoid clashes with when alpha renaming.
      add(fv2, get_free_indices(R1));
      int new_index = max(fv2)+1;

      // Do the alpha renaming
      foreach(i,overlap)
	rename_lambda(R1,*i,new_index++);

      E1 = dynamic_pointer_cast<expression>(R1);
    }
  }

  // Since this is an expression, substitute into sub-expressions
  for(int i=0;i<E1->size();i++)
    do_substitute(E1->sub[i], D, R2);
}

expression_ref substitute(const expression_ref& R1, const expression_ref& D, const expression_ref& R2)
{
  expression_ref R1b = R1;
  do_substitute(R1b, D, R2);
  return R1b;
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
      return substitute(E->sub[1], L->dummy_index, arg);
  }

  // Allow applying non-lambda expressions to arguments.
  // We need this to apply variables that turn out to be functions.
  return expression_ref(new expression(R,arg));
}

expression_ref apply(const expression_ref& E,
		     const vector< expression_ref > args)
{
  expression_ref E2 = E;
  for(int i=0;i<args.size();i++)
    E2 = apply(E2,args[i]);
  return E2;
}

void find_named_parameters_(const expression_ref& R, vector<string>& names)
{
  assert(R);
  // If this is a parameter, then makes sure we've got its name.
  if (shared_ptr<const parameter> n = dynamic_pointer_cast<const parameter>(R))
  {
    if (not includes(names,n->parameter_name))
      names.push_back(n->parameter_name);
  }

  // If this is an expression, check its sub-objects
  else if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      find_named_parameters_(E->sub[i], names);
  }
}

vector<string> find_named_parameters(const expression_ref& e)
{
  vector<string> names;
  find_named_parameters_(e,names);
  return names;
}

expression_ref Tuple(int n)
{
  return lambda_expression( data_function("Tuple",n) );
}

expression_ref Cons = lambda_expression( right_assoc_data_function(":",2) );

expression_ref ListEnd = lambda_expression( data_function("[]",0) );

#include "computation.H"

struct FreeOperationArgs: public OperationArgs
{
  const Context& C;
  const expression& E;

  boost::shared_ptr<const Object> evaluate(int slot);

  FreeOperationArgs* clone() const {return new FreeOperationArgs(*this);}

  FreeOperationArgs(const Context& C1,const expression& E1):C(C1),E(E1) { }
};

boost::shared_ptr<const Object> FreeOperationArgs::evaluate(int slot)
{
  return eval(C,E.sub[slot+1]);
}

expression_ref find_function_body(const Context& C, expression_ref& R)
{
  Function defun_f("defun",3,body_function_f);

  // For each function definition f x1..x[i]..xn | guard = body
  for(int i=0;i<C.F->size();i++)
  {
    expression_ref DR = (*C.F)[i];
    
    shared_ptr<const expression> DE = dynamic_pointer_cast<const expression>(DR);
    if (not DE) continue;
    if (defun_f.compare(*DE->sub[0]) != true) continue;
    if (DE->size() != 4) continue;
    
    expression_ref def = DE->sub[1];
    expression_ref guard = DE->sub[2];
    expression_ref body = DE->sub[3];
    
    vector<expression_ref> def_match_results;
    // 1. If R matches the def, then store the results.
    if (not eval_match(C,R,def,def_match_results,true)) continue;
    
    // 2. substitute the results into the guard expression
    guard = substitute(guard,def_match_results);
    
    //   ... and see if it evaluates to True
    vector<expression_ref> temp_results;
    if (not eval_match(C, guard, true, temp_results)) continue;
    
    // 3. Substitute the body
    body = substitute(body,def_match_results);
    
    return body;
  }
  return expression_ref();
}

expression_ref find_function_body(const Context& C, const expression_ref& R)
{
  expression_ref V (R->clone());
  return find_function_body(C,V);
}

// Contexts (can) allow three things
// 1. Variables to have values
// 2. Caching
// 3. Functions to have bodies.
// It would be nice to separate these three things into more abstract interfaces.

expression_ref eval(const Context& C, const expression_ref& R)
{
  expression_ref R2 = R;
  vector<expression_ref> results;
  eval_match(C,R2,expression_ref(),results);
  return R2;
}

// problem: expression_ref is currently designed to be unmodifiable.

bool eval_match(const Context& C, expression_ref& R, const expression_ref& Q, std::vector<expression_ref>& results, bool no_eval_top_level)
{
  // -1. If we are matching against a match expression, then succeed and store the result if asked.
  if (Q)
    if (shared_ptr<const match> M = dynamic_pointer_cast<const match>(Q))
    {
      if (M->index >= 0)
      {
	if (results.size() < M->index+1) results.resize(M->index+1);
	
	if (results[M->index]) throw myexception()<<"Match expression contains match index "<<M->index<<"' more than once!";
	
	results[M->index] = expression_ref(R->clone());
      }
      
      return true;
    }

  // 0. If R is not an expression
  shared_ptr<expression> RE = dynamic_pointer_cast<expression>(R);
  if (not RE)
  {
    if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(R))
      R = C.get_parameter_value(P->parameter_name);
    else if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R))
      throw myexception()<<"Cannot evaluate dummy variables!";
    else if (shared_ptr<const match> M = dynamic_pointer_cast<const match>(R))
      throw myexception()<<"Cannot evaluate match variables!";
    
    if (not Q) return true;

    // Do we have to do this?
    vector<expression_ref> results2 = results; 
    if (find_match(Q,R,results))
    {
      results = results2;
      return true;
    }
    else
      return false;
  }

  if (RE->size() == 1) throw myexception()<<"Expression '"<<R<<"' with only one element is not allowed!";

  // 1. Evaluate the head
  expression_ref head = eval(C,RE->sub[0]);
  if (head != RE->sub[0])
  {
    // This should affect R.
    RE->sub[0] = head;
  }

  // 2. If head is a lambda, then this is a lambda expression.  It evaluates to itself.
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(head);
  if (L and not Q) return true;
    
  // 3. If head is an expression, then apply the expression to E->sub[1]
  shared_ptr<const expression> RE2 = dynamic_pointer_cast<const expression>(head);
  if (RE2)
  {
    shared_ptr<const lambda> L2 = dynamic_pointer_cast<const lambda>(RE2->sub[0]);
    if (not L2)
      throw myexception()<<"Can't eval_match expression '"<<RE->print()<<"' with head = '"<<RE2->print()<<"'";

    if (RE->size() > 2)
      throw myexception()<<"Expression '"<<RE->print()<<"' applies a lambda function to more than one argument.";

    // FIXME - is this enough evaluation?
    R = substitute(RE2->sub[1], L2->dummy_index, RE->sub[1]);
    return eval_match(C,R,Q,results);
  }

  shared_ptr<const Function> RF = dynamic_pointer_cast<const Function>(head);

  // 4. If the head is a constructor, evaluate_match its arguments
  if (no_eval_top_level or (RF and RF->what_type == data_function_f) or L)
  {
    shared_ptr<const expression> QE;
    if (Q)
    {
      // Q must be an expression also.
      QE = dynamic_pointer_cast<const expression>(Q);
      if (not QE) return false;

      // Q must have the same number of arguments
      if (RE->size() != QE->size()) return false;

      // Q must have the same head here.
      // FIXME: There is no matching or evaluation of the head, here. (Will there be, later?)
      if (head->compare(*QE->sub[0]) != true)
	return false;
    }

    // If all the arguments match, then the whole expression matches
    for(int i=1;i<RE->size();i++)
    {
      expression_ref Q_sub;
      if (QE) Q_sub = QE->sub[i];
      if (not eval_match(C, RE->sub[i], Q_sub, results))
	return false;
    }

    return true;
  }

  // 5. If the head is a function, eval_match the substituted body
  else if (RF and RF->what_type == body_function_f)
  {
    expression_ref body = find_function_body(C,R);
    if (body) {
      R = body;
      return eval_match(C,R,Q,results);
    }
    else
      throw myexception()<<"No function definition for expression '"<<R->print()<<"'";
  }
  // 6. If the head is an Operation, evaluate the operation.
  else if (shared_ptr<const Operation> O = dynamic_pointer_cast<const Operation>(head))
  {
    FreeOperationArgs Args(C,*RE);

    // recursive calls to evaluate happen in here.
    shared_ptr<const Object> new_result;
    try{
      R = (*O)(Args);
      return eval_match(C,R,Q,results);
    }
    catch(myexception& e)
    {
      e.prepend("Evaluating expression '"+R->print()+"':\n");
      throw e;
    }
  }
  else
    throw myexception()<<"Don't know how to evaluate expression '"<<R->print()<<"'";
}

expression_ref _ = match(-1);
expression_ref _1 = match(0);
expression_ref _2 = match(1);
expression_ref _3 = match(2);
expression_ref _4 = match(3);

expression_ref default_value = lambda_expression(data_function("default_value",2));

expression_ref bounds = lambda_expression(data_function("bounds",2));

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( data_function("prob_density",2) );

// Fields: (prob_density) (random vars) (parameter expressions)
expression_ref distributed = lambda_expression( data_function("~",2) );

expression_ref sys_print = lambda_expression( Print() );

expression_ref concat = lambda_expression( Concat() );

expression_ref prob = lambda_expression( data_function("probability",1) );

expression_ref If = lambda_expression( IfThenElse() );

expression_ref defun = lambda_expression( data_function("defun",3) );

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
  sub[0] = data_function("Tuple",v.size());
  for(int i=0;i<v.size();i++)
    sub[i+1] = v[i];

  return expression_ref(expression(sub));
}

expression_ref get_list(const vector<expression_ref>& v)
{
  expression_ref E = ListEnd;

  for(int i=v.size()-1;i>=0;i++)
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
  // FIXME: merge with existing let expression...

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

  assert(dynamic_pointer_cast<const Function>(E->sub[0]));

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
    expression* E = new expression( case_obj() );
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

    assert(dynamic_pointer_cast<const Function>(PE->sub[0]));
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

expression_ref multi_case_expression(bool decompose, const vector<expression_ref>& terms, const vector<expression_ref>& patterns, 
				     const expression_ref& body, const expression_ref& otherwise)
{
  assert(terms.size() == patterns.size());
  std::set<int> free;
  for(int i=0;i<terms.size();i++)
    add(free, get_free_indices(terms[i]));

  std::set<int> free_patterns;
  for(int i=0;i<patterns.size();i++)
    add(free_patterns, get_pattern_indices(patterns[i]));

  assert(intersection(free, free_patterns).empty());
  std::set<int> free_body = get_free_indices(body);
  remove(free_body, free_patterns);

  expression_ref R = body;
  for(int i=patterns.size()-1; i>=0; i--)
    R = case_expression(decompose, terms[i],patterns[i],R,otherwise);

  return R;
}

expression_ref def_function(bool decompose, const vector< vector<expression_ref> >& patterns, const vector<expression_ref>& bodies, const expression_ref& otherwise)
{

  // Find the first safe var index
  std::set<int> free;
  if (otherwise)
    free = get_free_indices(otherwise);

  for(int i=0;i<patterns.size();i++)
  {
    add(free, get_free_indices(bodies[i]));
  
    for(int j=0; j<patterns[i].size(); j++)
      add(free, get_pattern_indices(patterns[i][j]));
  }
  
  int var_index = 0;
  if (not free.empty()) var_index = max(free)+1;

  // All versions of the function must have the same arity
  assert(patterns.size());
  for(int i=1;i<patterns.size();i++)
    assert(patterns[0].size() == patterns[i].size());

  // Construct the dummies
  vector<expression_ref> terms;
  for(int i=0;i<patterns[0].size();i++)
    terms.push_back(dummy(var_index+i));
    
  // Construct the case expression
  expression_ref R = otherwise;
  for(int i=patterns.size()-1; i>=0; i--)
    R = multi_case_expression(decompose, terms, patterns[i], bodies[i], R);

  // Turn it into a function
  for(int i=patterns[0].size()-1;i>=0;i--)
    R = expression(lambda(var_index+i),R);

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

expression_ref launchbury_normalize(const expression_ref& R)
{
  // 1. Var
  if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R))
    return R;
  
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E) return R;
  
  // 2. Lambda
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]);
  if (L)
  {
    assert(E->size() == 2);
    expression* V = new expression(*E);
    V->sub[1] = launchbury_normalize(E->sub[1]);

    if (V->sub[1] == E->sub[1])
      return R;
    else
      return V;
  }

  // 3. Application
  if (dynamic_pointer_cast<const expression>(E->sub[0]) or dynamic_pointer_cast<const dummy>(E->sub[0]))
  {
    assert(E->size() == 2);
    if (dynamic_pointer_cast<const dummy>(E->sub[1]))
    { 
      expression* V = new expression(*E);
      V->sub[0] = launchbury_normalize(E->sub[0]);
      return V;
    }
    else
    {
      int var_index = get_safe_binder_index(R);
      expression_ref x = dummy(var_index);

      return let_expression(x, launchbury_normalize(E->sub[1]), launchbury_normalize(E->sub[0])(x));
    }
  }
  
  // 4. Constructor
  if (dynamic_pointer_cast<const Function>(E->sub[0]) or 
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
      if (dynamic_pointer_cast<const dummy>(E->sub[i]))
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

  // 6. Case
  shared_ptr<const case_obj> Case = dynamic_pointer_cast<const case_obj>(E->sub[0]);
  if (Case)
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

  std::cerr<<"I don't recognize expression '"+ R->print() + "'\n";
  return R;
}
