#include "expression.H"
#include "util.H"
#include "operation.H"
#include "formula.H"
#include "model.H"

using boost::shared_ptr;
using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

vector<string> expression::print_arg_expressions() const 
{
  vector<string> arg_names;
  for(int i=0;i<args.size();i++)
    arg_names.push_back( args[i]->print() );
  
  return arg_names;
}

string expression::print() const 
{
  string result;
  if (head) {
    if (const Operator* O = dynamic_cast<const Operator*>(&*head))
      return O->print_expression(print_arg_expressions());
    if (const lambda* L = dynamic_cast<const lambda*>(&*head))
    {
      return "(lambda "+convertToString(L->dummy)+")("+args[0]->print()+")";
    }
    else
      result = head->print();
  }
  return print_operator_expression(result,print_arg_expressions());
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

expression::expression(const Object& O)
 :head(O.clone()) 
{}

expression::expression(const shared_ptr<const Object>& O)
 :head(O)
{}

expression::expression(const shared_ptr<const Object>& O, const expression_ref arg)
 :head(O) 
{
  args.push_back(arg);
}

tribool constant::compare(const Object& o) const 
{
  const constant* E = dynamic_cast<const constant*>(&o);
  if (not E) 
    return false;

  return value->compare(*E->value);
}

expression_ref::expression_ref(const term_ref& t)
  :shared_ptr<const expression>(t.F->terms[t.index].E)
{}

tribool dummy_expression::compare(const Object& o) const {
  const dummy_expression* E = dynamic_cast<const dummy_expression*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

string dummy_expression::print() const {
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
bool find_match(const expression_ref& pattern, const expression_ref& E, vector<shared_ptr<const expression> >& results)
{
  // if this is a match expression, then succeed, and store E as the result of the match
  shared_ptr<const match> M = dynamic_pointer_cast<const match>(pattern->head);
  if (M) 
  {
    assert(pattern->n_args() == 0);
    
    if (M->index >= 0)
    {
      if (results.size() < M->index+1) results.resize(M->index+1);

      if (results[M->index]) 
	throw myexception()<<"Match expression '"<<pattern->print()<<"' contains match index "<<M->index<<"' more than once!";
      results[M->index] = E;
    }

    return true;
  }

  // 
  if (pattern->n_args() != E->n_args()) return false;

  // The heads have to compare equal.  There is no matching there. (Will there be, later?)
  if (pattern->head->compare(*E->head) != true)
    return false;

  for(int i=0;i<pattern->n_args();i++)
    if (not find_match(pattern->args[i], E->args[i], results))
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

vector< shared_ptr< const expression > > model_args(const Model& M)
{
  vector< shared_ptr< const expression > > args;

  for(int i=0;i<M.n_parameters();i++) 
  {
    args.push_back( shared_ptr< const expression >(new expression(parameter(M.parameter_name(i)) ) ) );
  }

  return args;
}

lambda::lambda(int d)
  :dummy(d)
{ }

expression_ref lambda_expression(const Operator& O)
{
  int n = O.n_args();
  assert(n != -1);
  
  vector< shared_ptr<const expression> > A;
  for(int i=0;i<n;i++)
    A.push_back(shared_ptr<const expression>(new dummy_expression(i)));
  
  shared_ptr<const expression> E(new expression(O, A));
  
  for(int i=n-1;i>=0;i--) 
  {
    vector<shared_ptr<const expression> > args(1,E);
    E = shared_ptr<const expression>(new expression(lambda(i),args));
  }
  
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

Function annotation_function(const std::string& s, int n)
{
  return Function(s, n, annotation_f);
}

shared_ptr<const expression> substitute(shared_ptr<const expression> E1, int dummy, shared_ptr<const expression> E2)
{
  vector<shared_ptr<const expression> > args(E1->n_args());
  bool found = false;
  for(int i=0;i<E1->n_args();i++)
  {
    args[i] = substitute(E1->args[i],dummy,E2);
    if (args[i] != E1->args[i]) found = true;
  }

  if (shared_ptr<const dummy_expression> D = dynamic_pointer_cast<const dummy_expression>(E1))
  {
    assert(E1->n_args() == 0);
    if (D->index == dummy)
      return E2;
    else
      return E1;
  }

  // This is not a dummy expression, and the arguments (we didn't search head) do not contain the dummy being replaced;
  if (not found) return E1;

  // make sure we don't try to substitute for quantified dummies
  if (shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E1->head))
  {
    if (L->dummy == dummy)
      throw myexception()<<"Trying to substitution for dummy "<<dummy<<" in lambda express that quantifies it!";
  }

  return shared_ptr<const expression>(new expression(E1->head,args));
}

shared_ptr<const expression> apply(const expression_ref& E,const expression_ref& arg)
{
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->head);
  if (not L)
    throw myexception()<<"Too many arguments to expression "<<E->print()<<".  (Is this a function at all?)";

  return substitute(E->args[0], L->dummy, arg);
}

shared_ptr<const expression> apply(const expression_ref& E,
				   const vector<shared_ptr<const expression> > args,
				   int i)
{
  shared_ptr<const expression> result1 = apply(E,args[i]);

  if (i<args.size())
    result1 = apply(result1, args, i+1);

  return result1;
}

shared_ptr<const expression> apply(const expression_ref& E,
				   const vector<shared_ptr<const expression> > args)
{
  return apply(E,args,0);
}

void find_named_parameters_(shared_ptr<const expression> e, vector<string>& names)
{
  if (shared_ptr<const parameter> n = dynamic_pointer_cast<const parameter>(e->head)) 
  {
    if (not includes(names,n->parameter_name))
      names.push_back(n->parameter_name);
  }
  else if (shared_ptr<const Operator> o = dynamic_pointer_cast<const Operator>(e->head)) 
  {
    for(int i=0;i<e->args.size();i++)
      find_named_parameters_(e->args[i], names);
  }
}

vector<string> find_named_parameters(shared_ptr<const expression> e)
{
  vector<string> names;
  find_named_parameters_(e,names);
  return names;
}

