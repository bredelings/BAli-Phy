#include "expression.H"
#include "util.H"
#include "operation.H"
#include "formula.H"
#include "model.H"

using boost::shared_ptr;
using std::vector;
using std::string;

term_ref_expression::term_ref_expression(const term_ref& r):term(r) {}
term_ref_expression::term_ref_expression(int i, boost::shared_ptr<const Formula> f):term(i,f) {}


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

// operator expression

vector<string> operator_expression::print_arg_expressions() const 
{
  vector<string> arg_names;
  for(int i=0;i<args.size();i++)
    arg_names.push_back( args[i]->print() );
  
  return arg_names;
}

shared_ptr<const expression> operator_expression::substitute(int dummy, shared_ptr<const expression> E) const
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
  
  shared_ptr<operator_expression> result;
  
  if (change)
  {
    result = shared_ptr<operator_expression>( clone() );
    result->args = new_args;
  }
  
  return result;
}

int operator_expression::highest_unused_dummy() const
{
  int highest = 0;
  for(int i=0;i<args.size();i++)
    highest = std::max(highest, args[i]->highest_unused_dummy());
  return highest;
}

string operator_expression::print() const 
{
  return get_operator()->print_expression( print_arg_expressions() );
}

operator_expression::operator_expression(const vector< shared_ptr<const expression> >& A)
  :expression(A)
{ }

// operation expression

operation_expression::operation_expression(const Operation& O,const vector< shared_ptr<const expression> >& A)
  :operator_expression(A),
   op(O.clone())
{ }

operation_expression::operation_expression(shared_ptr<const Operation> O,const vector< shared_ptr<const expression> >& A)
  :operator_expression(A),
   op(O->clone())
{ }

vector< shared_ptr< const expression > > model_args(const Model& M)
{
  vector< shared_ptr< const expression > > args;

  for(int i=0;i<M.n_parameters();i++) 
  {
    args.push_back( shared_ptr< const expression> ( new named_parameter_expression(M.parameter_name(i) ) ) );
  }

  return args;
}

model_expression::model_expression(const Model& M)
  :operator_expression( model_args(M) ),
   m(M.clone())
{ }

lambda_expression::lambda_expression(const Operation& O)
  :dummy_variable(0)
{
  int n = O.n_args();
  assert(n != -1);
  
  vector< shared_ptr<const expression> > A;
  for(int i=0;i<n;i++)
    A.push_back(shared_ptr<const expression>(new dummy_expression(i)));
  
  shared_ptr<const expression> E(new operation_expression(O, A));
  
  for(int i=n-1;i>0;i--)
    E = shared_ptr<const expression>(new lambda_expression(i,E));
  
  quantified_expression = E;
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
  if (shared_ptr<const named_parameter_expression> n = boost::dynamic_pointer_cast<const named_parameter_expression>(e)) 
  {
    if (not includes(names,n->parameter_name))
      names.push_back(n->parameter_name);
  }
  else if (shared_ptr<const operator_expression> o = boost::dynamic_pointer_cast<const operator_expression>(e)) 
  {
    for(int i=0;i<o->args.size();i++)
      find_named_parameters_(o->args[i], names);
  }
}

vector<string> find_named_parameters(shared_ptr<const expression> e)
{
  vector<string> names;
  find_named_parameters_(e,names);
  return names;
}

