#include "expression.H"
#include "util.H"
#include "operation.H"
#include "formula.H"

using boost::shared_ptr;
using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

vector<string> print_arg_expressions(const expression& e)
{
  vector<string> arg_names;
  for(int i=0;i<e.args.size();i++)
    arg_names.push_back( e.args[i]->print() );
  
  return arg_names;
}

// How do I make constructor-specific methods of printing data expressions?
// Can I move to defining the print function using an expression?
string expression::print() const 
{
  string result;
  assert(head);

  if (const Operator* O = dynamic_cast<const Operator*>(&*head))
    return O->print_expression(print_arg_expressions(*this));

  if (const lambda* L = dynamic_cast<const lambda*>(&*head))
    return "(lambda "+convertToString(L->dummy_index)+")("+args[0]->print()+")";

  else
    return print_operator_expression(head->print(),print_arg_expressions(*this));
}

tribool expression::compare(const Object& o) const 
{
  const expression* E = dynamic_cast<const expression*>(&o);
  if (not E) 
    return false;

  tribool same = head->compare(*E->head);

  if (not same) return false;

  if (n_args() != E->n_args()) return false;

  for(int i=0;i<n_args();i++) {
    same = same and args[i]->compare(*E->args[i]);
    if (not same) return false;
  }

  return same;
}

expression::expression(const object_ref& O)
  :head(O)
{}

expression::expression(const object_ref& O, const expression_ref& arg)
 :head(O) 
{
  args.push_back(arg);
}

expression::expression(const object_ref& O, const std::vector< expression_ref >& A)
 :head(O), args(A)
{ }

tribool constant::compare(const Object& o) const 
{
  const constant* E = dynamic_cast<const constant*>(&o);
  if (not E) 
    return false;

  return value->compare(*E->value);
}

expression_ref::expression_ref(const term_ref& t)
  :shared_ptr<const Object>((*t.F)[t.index])
{}

tribool dummy::compare(const Object& o) const {
  const dummy* E = dynamic_cast<const dummy*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

string dummy::print() const {
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
  if (pattern_exp->n_args() != E_exp->n_args()) return false;

  // The heads have to compare equal.  There is no matching there. (Will there be, later?)
  if (pattern_exp->head->compare(*E_exp->head) != true)
    return false;

  for(int i=0;i<pattern_exp->n_args();i++)
    if (not find_match(pattern_exp->args[i], E_exp->args[i], results))
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

lambda::lambda(int d)
  :dummy_index(d)
{ }

expression_ref lambda_expression(const Operator& O)
{
  int n = O.n_args();
  assert(n != -1);
  
  vector< expression_ref > A;
  for(int i=0;i<n;i++)
    A.push_back(expression_ref(new dummy(i)));
  
  expression_ref E(new expression(O, A));
  
  for(int i=n-1;i>=0;i--) 
    E = expression_ref(new expression(lambda(i),E));
  
  return E;
}

tribool Function::compare(const Object& o) const
{
  const Function* E = dynamic_cast<const Function*>(&o);
  if (not E) 
    return false;

  return f_name == E->f_name;
}

Function::Function(const string& s, int n, function_type_t f_t)
  :f_name(s), n_args_(n), what_type(f_t)
{
  
}

Function data_function(const std::string& s, int n)
{
  return Function(s, n, data_function_f);
}

expression_ref substitute(const expression_ref& R1, int dummy_index, const expression_ref& R2)
{
  // If this is the relevant dummy, then substitute
  if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R1))
  {
    if (D->index == dummy_index)
      return R2;
    else
      return R1;
  }

  shared_ptr< const expression> E1 = dynamic_pointer_cast<const expression>(R1);

  // If this is any other constant, then it doesn't contain the dummy
  if (not E1) return R1;

  // If this is an expression, then compute the substituted args
  vector< expression_ref > args(E1->n_args());
  bool found = false;
  for(int i=0;i<E1->n_args();i++)
  {
    args[i] = substitute(E1->args[i], dummy_index, R2);
    if (args[i] != E1->args[i]) found = true;
  }

  // This is not a dummy expression, and the arguments (we didn't search head) do not contain the dummy being replaced;
  if (not found) return R1;

  // make sure we don't try to substitute for quantified dummies
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E1->head))
  {
    if (L->dummy_index == dummy_index)
      throw myexception()<<"Trying to substitution for dummy "<<dummy_index<<" in lambda express that quantifies it!";
  }

  // Construct a new expression containing the substituted args.
  return expression_ref(new expression(E1->head,args));
}

expression_ref substitute(const expression_ref& R1, const object_ref& D, const expression_ref& R2)
{
  // If this is the relevant dummy, then substitute
  if (D->compare(*R1))
    return R2;

  shared_ptr< const expression> E1 = dynamic_pointer_cast<const expression>(R1);

  // If this is any other constant, then it doesn't contain the dummy
  if (not E1) return R1;

  // If this is an expression, then compute the substituted args
  vector< expression_ref > args(E1->n_args());
  bool found = false;
  for(int i=0;i<E1->n_args();i++)
  {
    args[i] = substitute(E1->args[i], D, R2);
    if (args[i] != E1->args[i]) found = true;
  }

  // This is not a dummy expression, and the arguments (we didn't search head) do not contain the dummy being replaced;
  if (not found) return R1;

  // make sure we don't try to substitute for quantified dummies
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E1->head))
  {
    if (D->compare(dummy(L->dummy_index)))
      throw myexception()<<"Trying to substitution for dummy "<<L->dummy_index<<" in lambda express that quantifies it!";
  }

  // Construct a new expression containing the substituted args.
  return expression_ref(new expression(E1->head,args));
}

expression_ref apply(const expression_ref& R,const expression_ref& arg)
{
  assert(R);

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E)
    throw myexception()<<"Too many arguments to constant "<<R->print()<<".";

  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->head);
  if (not L)
    throw myexception()<<"Too many arguments to expression "<<E->print()<<".  (Is this a function at all?)";

  return substitute(E->args[0], L->dummy_index, arg);
}

expression_ref apply(const expression_ref& E,
		     const vector< expression_ref > args,
		     int i)
{
  expression_ref result1 = apply(E,args[i]);

  if (i<args.size())
    result1 = apply(result1, args, i+1);

  return result1;
}

expression_ref apply(const expression_ref& E,
		     const vector< expression_ref > args)
{
  return apply(E,args,0);
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
    for(int i=0;i<E->args.size();i++)
      find_named_parameters_(E->args[i], names);
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

expression_ref Cons = lambda_expression( data_function("Cons",2) );

expression_ref ListEnd = lambda_expression( data_function("[]",0) );

#include "computation.H"

struct FreeOperationArgs: public OperationArgs
{
  const expression& E;

  boost::shared_ptr<const Object> evaluate(int slot);

  FreeOperationArgs* clone() const {return new FreeOperationArgs(*this);}

  FreeOperationArgs(const expression& E1):E(E1) { }
};

boost::shared_ptr<const Object> FreeOperationArgs::evaluate(int slot)
{
  return eval(E.args[slot]);
}

// Contexts (can) allow three things
// 1. Variables to have values
// 2. Caching
// 3. Functions to have bodies.
// It would be nice to separate these three things into more abstract interfaces.

expression_ref eval(const expression_ref& R)
{
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  if (not E)
  {
    if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(R))
      throw myexception()<<"Cannot evaluate parameters w/o a context ('"<<P->print()<<"')";
    else if (shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(R))
      throw myexception()<<"Cannot evaluate dummy variables!";
    else if (shared_ptr<const match> D = dynamic_pointer_cast<const match>(R))
      throw myexception()<<"Cannot evaluate dummy variables!";

    // This is a literal constant
    else
      return R;
  }

  // If the expression is a function expression...
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->head);
  if (L)
    return R;

  // If the expression is a data function expression, evaluate its arguments
  shared_ptr<const Function> f = dynamic_pointer_cast<const Function>(E->head);
  if (f)
  {
    if (f->what_type != data_function_f)
      throw myexception()<<"Cannot evaluate non-data functions without a context!";

    vector< expression_ref > args(E->n_args());
    for(int i=0;i<args.size();i++)
      args[i] = eval(E->args[i]);

    return expression_ref(new expression(f,args));
  }

  // Hey, how about a model expression?

  // Otherwise the expression must be an op expression
  shared_ptr<const Operation> O = dynamic_pointer_cast<const Operation>(E->head);
  assert(O);
  
  FreeOperationArgs Args(*E);

  // recursive calls to evaluate happen in here.
  shared_ptr<const Object> new_result;
  try{
    return (*O)(Args);
  }
  catch(myexception& e)
  {
    e.prepend("Evaluating expression '"+R->print()+"':\n");
    throw e;
  }
}

expression_ref eval_match(const expression_ref& R, const expression_ref& Q, std::vector<expression_ref>& results)
{
  // If we are matching against a match expression, then succeed and store the result if asked.
  if (shared_ptr<const match> M = dynamic_pointer_cast<const match>(Q))
  {
    if (M->index >= 0)
    {
      if (results.size() < M->index+1) results.resize(M->index+1);

      if (results[M->index]) throw myexception()<<"Match expression contains match index "<<M->index<<"' more than once!";

      results[M->index] = expression_ref(R->clone());
    }

    return R;
  }

  shared_ptr<const expression> QE = dynamic_pointer_cast<const expression>(Q);
  shared_ptr<const expression> RE = dynamic_pointer_cast<const expression>(R);

  // If the eval expression or the query expression is a literal constant, just evaluate R and see if it matches.
  // Do the same if the eval expression is an object returned from an operation
  if (not QE or not RE or dynamic_cast<const Operation*>(&*RE->head))
  {
    expression_ref result = eval(R);
    vector<expression_ref> results2 = results; // FIXME!  This is a bit expensive...
    if (find_match(result,Q,results2))
    {
      std::swap(results,results2);
      return result;
    }
    else
      return expression_ref();
  }
  
  shared_ptr<const Function> QF = dynamic_pointer_cast<const Function>(QE->head);
  if (not QF)
    throw myexception()<<"eval_match: can't attempt to match expression '"<<R->print()<<"': head must be function.";

  if (QF->what_type == data_function_f)
  {
    // Expressions must have the same number of arguments
    if (QE->n_args() != RE->n_args()) return false;

    // The heads have to compare equal.  There is no matching there. (Will there be, later?)
    if (QE->head->compare(*RE->head) != true)
      return false;

    // If all the arguments match, then the whole expression matches
    vector<expression_ref> args(QE->n_args());
    for(int i=0;i<QE->n_args();i++)
    {
      args[i] = eval_match(RE->args[i],QE->args[i],results);
      if (not args[i])
	return expression_ref();
    }

    return expression_ref(new expression(QE->head,args));
  }
  else
  {
    /*
    for(int i=0;i<definitions.size();i++)
    {
      // For each function definition f x1..x[i]..xn | guard = body
      // if (R eval_matches to (f x1 ..x[n].. xn) leading to results=results_func
      // substitute the guard, and see if it eval_matches ~ True
      // substitute the body, and see if it eval_matches ~ Q
    }
    */
    throw myexception()<<"You can't match against an evaluated function, only a data constructor!";
  }
}

