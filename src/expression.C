#include "expression.H"
#include "util.H"
#include "operation.H"
#include "formula.H"
#include "model.H"

using boost::shared_ptr;
using std::vector;
using std::string;


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

shared_ptr<const expression> expression::substitute(int dummy, shared_ptr<const expression> E) const
{
  // construct the substituted arg list, and see if 
  vector< shared_ptr<const expression> > new_args(args.size());
  bool change = false;
  for(int i=0;i<args.size();i++)
  {
    new_args[i] = ::substitute(args[i], dummy, E);
    if (new_args[i] != args[i])
      change = true;
  }
  
  shared_ptr<expression> result;
  
  if (change)
  {
    result = shared_ptr<expression>( clone() );
    result->args = new_args;
  }
  
  return result;
}

int expression::highest_unused_dummy() const
{
  int highest = 0;
  for(int i=0;i<args.size();i++)
    highest = std::max(highest, args[i]->highest_unused_dummy());
  return highest;
}

tribool constant::compare(const Object& o) const 
{
  const constant* E = dynamic_cast<const constant*>(&o);
  if (not E) 
    return false;

  return value->compare(*E->value);
}

expression_ref::expression_ref(const term_ref& t)
  :boost::shared_ptr<const expression>(t.F->terms[t.index].E)
{}

tribool dummy_expression::compare(const Object& o) const {
  const dummy_expression* E = dynamic_cast<const dummy_expression*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

shared_ptr<const expression> dummy_expression::substitute(int dummy, shared_ptr<const expression> E) const
{
  if (index == dummy) 
    return E;
  else
    return shared_ptr<const expression>();
}

string dummy_expression::print() const {
  return string("#")+convertToString(index);
}

tribool match_expression::compare(const Object& o) const 
{
  const match_expression* E = dynamic_cast<const match_expression*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

string match_expression::print() const 
{
  if (index == -1) 
    return "_";
  else
    return string("_")+convertToString(index);
}


// How would we handle lambda expressions, here?
bool match(const expression_ref& pattern, const expression_ref& E, vector<shared_ptr<const expression> >& results)
{
  // if this is a match expression, then succeed, and store E as the result of the match
  shared_ptr<const match_expression> M = dynamic_pointer_cast<const match_expression>(pattern);
  if (M) 
  {
    assert(pattern->n_args() == 0);
    
    if (results.size() < M->index) results.resize(M->index+1);

    if (results[M->index]) 
      throw myexception()<<"Match expression '"<<pattern->print()<<"' contains match index "<<M->index<<"' more than once!";

    results[M->index] = E;

    return true;
  }

  // 
  if (pattern->n_args() != E->n_args()) return false;

  // The heads have to compare equal.  There is no matching there. (Will there be, later?)
  if (pattern->head->compare(*E->head) != true)
    return false;

  for(int i=0;i<pattern->n_args();i++)
    if (not match(pattern->args[i], E->args[i], results))
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

lambda_expression::lambda_expression(const Operation& O)
  :dummy_variable(0)
{
  int n = O.n_args();
  assert(n != -1);
  
  vector< shared_ptr<const expression> > A;
  for(int i=0;i<n;i++)
    A.push_back(shared_ptr<const expression>(new dummy_expression(i)));
  
  shared_ptr<const expression> E(new expression(O, A));
  
  for(int i=n-1;i>0;i--)
    E = shared_ptr<const expression>(new lambda_expression(i,E));
  
  quantified_expression = E;
}

lambda_expression::lambda_expression(const Function& F)
  :dummy_variable(0)
{
  int n = F.n_args;
  assert(n != -1);
  
  vector< shared_ptr<const expression> > A;
  for(int i=0;i<n;i++)
    A.push_back(shared_ptr<const expression>(new dummy_expression(i)));
  
  shared_ptr<const expression> E(new expression(F, A));
  
  for(int i=n-1;i>0;i--)
    E = shared_ptr<const expression>(new lambda_expression(i,E));
  
  quantified_expression = E;
}

tribool Function::compare(const Object& o) const
{
  const Function* E = dynamic_cast<const Function*>(&o);
  if (not E) 
    return false;

  return f_name == E->f_name;
}

Function::Function(const string& s, int n, function_type_t f_t)
  :f_name(s), n_args(n), what_type(f_t)
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

shared_ptr<const expression> lambda_expression::substitute(int dummy, shared_ptr<const expression> E) const
{
  if (dummy_variable.index == dummy)
    throw myexception()<<"Trying to substitution for dummy "<<dummy<<" in lambda express that quantifies it!";

  shared_ptr<const expression> result = quantified_expression->substitute(dummy,E);
  
  if (not result) return result;
  
  return shared_ptr<const expression>(new lambda_expression(dummy_variable.index,result));
}

shared_ptr<const expression> substitute(shared_ptr<const expression> E1, int dummy, shared_ptr<const expression> E2)
{
  shared_ptr<const expression> E3 = E1->substitute(dummy,E2);
  if (E3) 
    return E3;
  else
    return E1;
}

shared_ptr<const expression> apply(const expression& E,shared_ptr<const expression> arg)
{
  const lambda_expression* lambda = dynamic_cast<const lambda_expression*>(&E);
  if (not lambda)
    throw myexception()<<"Too many arguments to expression "<<E.print()<<".  (Is this a function at all?)";

  return substitute(lambda->quantified_expression, lambda->dummy_variable.index, arg);
}

shared_ptr<const expression> apply(const expression& E,const expression& arg)
{
  return apply(E,shared_ptr<const expression>(arg.clone()));
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   shared_ptr<const expression> arg)
{
  return apply(*E,arg);
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   const expression& arg)
{
  return apply(*E,shared_ptr<const expression>(arg.clone()));
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   const vector<shared_ptr<const expression> > args,
				   int i)
{
  shared_ptr<const expression> result1 = apply(E,args[i]);

  if (i<args.size())
    result1 = apply(result1, args, i+1);

  return result1;
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   const vector<shared_ptr<const expression> > args)
{
  return apply(E,args,0);
}

shared_ptr<const expression> expression::apply(shared_ptr<const expression> arg) const
{
  return ::apply(*this,arg);
}

shared_ptr<const expression> expression::apply(const expression& arg) const
{
  return ::apply(*this,arg);
}

void find_named_parameters_(shared_ptr<const expression> e, vector<string>& names)
{
  if (shared_ptr<const parameter> n = boost::dynamic_pointer_cast<const parameter>(e->head)) 
  {
    if (not includes(names,n->parameter_name))
      names.push_back(n->parameter_name);
  }
  else if (shared_ptr<const Operator> o = boost::dynamic_pointer_cast<const Operator>(e->head)) 
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

