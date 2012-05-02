#include "operations.H"
#include "graph_register.H"
#include "exponential.H"

using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

closure Seq::operator()(OperationArgs& Args) const
{
  Args.lazy_evaluate(0);
  return Args.lazy_evaluate(1);
}

expression_ref seq = lambda_expression( Seq() );

closure Apply::operator()(OperationArgs& Args) const
{
  closure C = Args.lazy_evaluate(0);

  // We should assert this.
  object_ptr<const index_var> V = dynamic_pointer_cast<const index_var>(Args.reference(1));
  int arg = Args.current_closure().lookup_in_env( V->index );

  // We could actually change this to a static_cast.  C.exp MUST be an expression.  C.exp[0] MUST be a lambda.
  if (object_ptr<const expression> E = is_a(C.exp, lambda2()))
  {
    C.exp = E->sub[1];
    C.Env.push_back(arg);
    return C;
  }

  throw myexception()<<"Can't apply non-lambda object '"<<C.exp->print()<<"' to argument.'";
}

std::string Apply::name() const {
  return "@";
}

closure Case::operator()(OperationArgs& Args) const
{
  const closure& C = Args.current_closure();

  closure obj = Args.lazy_evaluate(0);

  int L = (Args.n_args() - 1)/2;

  vector<expression_ref> cases(L);
  vector<expression_ref> bodies(L);
  for(int i=0;i<L;i++)
  {
    cases[i] = Args.reference(1 + 2*i);
    bodies[i] = Args.reference(2 + 2*i);
  }

  object_ptr<const expression> obj_E = dynamic_pointer_cast<const expression>(obj.exp);
  assert(not obj_E or not dynamic_pointer_cast<const lambda2>(obj_E->sub[0]));

  closure result;
  result.Env = C.Env;

  for(int i=0;i<L and not result;i++)
  {
    // If its _, then match it.
    if (object_ptr<const dummy> D2 = dynamic_pointer_cast<const dummy>(cases[i]))
    {
      // We standardize to avoid case x of v -> f(v) so that f cannot reference v.
      assert(D2->index == -1);
      assert(i == L-1);

      result.exp = bodies[i];
    }
    // If obj is a 0-arg literal constant constructor, then match iff obj==cases[i]
    else if (not obj_E)
    {
      if (obj.exp->compare(*cases[i]))
	result.exp = bodies[i];
    }
    // If we are an n-arg constructor, then match iff the case is an expression and the head matches.
    else if (obj_E)
    {
      if (object_ptr<const expression> case_E = dynamic_pointer_cast<const expression>(cases[i]))
      {
	if (obj_E->sub[0]->compare(*case_E->sub[0]))
	{
	  // The number of constructor fields is the same the for case pattern and the case object.
	  assert(obj_E->size() == case_E->size());
	  // The number of entries in the environment is the same as the number of constructor fields.
	  assert(obj_E->size() == 1+obj.Env.size());

	  result.exp = bodies[i];

	  for(int j=1;j<obj_E->size();j++)
	  {
	    int index = dynamic_pointer_cast<const index_var>(obj_E->sub[j])->index;

	    result.Env.push_back( obj.lookup_in_env( index ) );
	  }
	}
      }
    }
  }

  if (not result)
    throw myexception()<<"Case: no valid alternative in '"<<make_case_expression(obj.exp, cases, bodies)<<"'";

  result = get_trimmed(result);

  return result;
}

std::string Case::name() const {
  return "case";
}

closure MkArray::operator()(OperationArgs& Args) const
{
  int n = *Args.evaluate_as<Int>(0);
  expression_ref f = Args.reference(1);

  // We can't do negative-sized arrays
  assert(n >= 0);
  // The function should be represented as a heap variable...
  assert(dynamic_pointer_cast<const index_var>(f));

  object_ptr<expression> exp = new expression;
  closure C;
  exp->sub.resize(n+1);
  exp->sub[0] = constructor("Array",n);
  for(int i=0;i<n;i++)
  {
    // change to C.exp <<= index_var(i)
    exp->sub.push_back(index_var(n - 1 - i));
    C.Env.push_back( Args.allocate((f,i)) );
  }
  C.exp = exp;
  
  return C;
}

expression_ref mkArray = lambda_expression( MkArray() );

closure GetIndex::operator()(OperationArgs& Args) const
{
  closure C = Args.lazy_evaluate(0);
  int n = *Args.evaluate_as<Int>(1);

  object_ptr<const expression> A = dynamic_pointer_cast<const expression>(C.exp);
  int N = A->sub.size()-1;

  if (n < 0 or n >= N)
    throw myexception()<<"Trying to access index "<<n<<" in array of size "<<N<<".";
      
  // Return a reference to the heap variable pointed to by the nth entry
  return {index_var(0), {C.Env[n]} };
}

expression_ref getIndex = lambda_expression( GetIndex() );

closure ArrayBounds::operator()(OperationArgs& Args) const
{
  object_ptr<const expression> A = convert<const expression>( Args.lazy_evaluate(0).exp );
  int N = A->sub.size()-1;

  return graph_normalize(Tuple(0,N-1));
}

expression_ref bounds = lambda_expression( ArrayBounds() );

closure LExp_Op::operator()(OperationArgs& Args) const
{
  object_ptr<const EigenValues> L = Args.evaluate_as<EigenValues>(0);
  // FIXME - Vector_from_Tuple_Op

  expression_ref pi_E = Args.evaluate(1);
  double t = *Args.evaluate_as<Double>(2);

  std::vector<double> pi = get_vector<double,Double>(pi_E);
  
  return object_ptr<const Object>(new MatrixObject( exp(*L, pi, t) ) );
}

expression_ref LExp = lambda_expression( LExp_Op() );

//---------------------------------------------------------------------------------------//

