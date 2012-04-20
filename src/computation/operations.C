#include "operations.H"
#include "graph_register.H"
#include "exponential.H"

using std::vector;
using std::string;

using boost::dynamic_pointer_cast;
using boost::shared_ptr;

object_ptr<const Object> Seq::operator()(OperationArgs& Args) const
{
  Args.lazy_evaluate(0);
  return Args.lazy_evaluate(1);
}

expression_ref seq = lambda_expression( Seq() );

object_ptr<const Object> Apply::operator()(OperationArgs& Args) const
{
  //FIXME - should this be lazy_evaluate?  Perhaps which a lambda function it doesn't matter.
  object_ptr<const Object> f = Args.evaluate(0);
  object_ptr<const Object> arg = Args.reference(1);

  if (object_ptr<const expression> fE = dynamic_pointer_cast<const expression>(f))
  {
    if (object_ptr<const lambda> fL = dynamic_pointer_cast<const lambda>(fE->sub[0]))
      return substitute(fE->sub[2], fE->sub[1], arg);
  }

  throw myexception()<<"Can't apply non-lambda object '"<<f->print()<<"' to argument '"<<arg->print()<<"'";
}

std::string Apply::name() const {
  return "@";
}

object_ptr<const Object> Case::operator()(OperationArgs& Args) const
{
  object_ptr<const Object> obj = Args.lazy_evaluate(0);
  object_ptr<const Object> alts = Args.reference(1);

  int L = (Args.n_args() - 1)/2;

  vector<expression_ref> cases(L);
  vector<expression_ref> results(L);
  for(int i=0;i<L;i++)
  {
    cases[i] = Args.reference(1 + 2*i);
    results[i] = Args.reference(2 + 2*i);
  }

  object_ptr<const expression> E = dynamic_pointer_cast<const expression>(obj);
  assert(not E or not dynamic_pointer_cast<const lambda>(E->sub[0]));

  expression_ref result;
  for(int i=0;i<L and not result;i++)
  {
    // If its a dummy, then match it.
    if (object_ptr<const dummy> D2 = dynamic_pointer_cast<const dummy>(cases[i]))
    {
      result = results[i];

      // Substitute the matched value into the expression if the dummy isn't "_";
      if (D2->index >= 0)
	result = substitute(result, cases[i], obj);
    }
    // If we are a 0-arg literal constant constructor, then match iff obj==cases[i]
    else if (not E)
    {
      if (obj->compare(*cases[i]))
	result = results[i];
    }
    // If we are an n-arg constructor, then match iff the case is an expression and the head matches.
    else if (E)
    {
      if (object_ptr<const expression> E2 = dynamic_pointer_cast<const expression>(cases[i]))
      {
	if (E->sub[0]->compare(*E2->sub[0]))
	{
	  assert(E->size() == E2->size());

	  result = results[i];

	  for(int j=1;j<E->size();j++)
	  {
	    if (not is_wildcard(E2->sub[j]))
	      result = substitute(result, E2->sub[j], E->sub[j]);
	  }
	}
      }
    }
  }

  if (not result)
    throw myexception()<<"Case: no valid alternative in '"<<make_case_expression(obj, cases, results)<<"'";

  return result;
}

std::string Case::name() const {
  return "case";
}

object_ptr<const Object> MkArray::operator()(OperationArgs& Args) const
{
  int n = *Args.evaluate_as<Int>(0);
  expression_ref f = Args.reference(1);

  // We can't do negative-sized arrays
  assert(n >= 0);
  // The function should be represented as a reg_var...
  assert(dynamic_pointer_cast<const reg_var>(f));

  object_ptr<expression> a ( new expression );
  a->sub.resize(n+1);
  a->sub[0] = constructor("Array",n);
  for(int i=0;i<n;i++)
  {
    int r = Args.allocate((f,i));
    
    a->sub[i+1] = reg_var(r);
  }
  
  return a;
}

expression_ref mkArray = lambda_expression( MkArray() );

object_ptr<const Object> GetIndex::operator()(OperationArgs& Args) const
{
  object_ptr<const expression> A = convert<const expression>( Args.lazy_evaluate(0) );
  int n = *Args.evaluate_as<Int>(1);
  int N = A->sub.size()-1;
  if (n < 0 or n >= N)
    throw myexception()<<"Trying to access index "<<n<<" in array of size "<<N<<".";
      
  return A->sub[1+n];
}

expression_ref getIndex = lambda_expression( GetIndex() );

object_ptr<const Object> ArrayBounds::operator()(OperationArgs& Args) const
{
  object_ptr<const expression> A = convert<const expression>( Args.lazy_evaluate(0) );
  int N = A->sub.size()-1;

  return graph_normalize(Tuple(0,N-1));
}

expression_ref bounds = lambda_expression( ArrayBounds() );

object_ptr<const Object> LExp_Op::operator()(OperationArgs& Args) const
{
  object_ptr<const EigenValues> L = Args.evaluate_as<EigenValues>(0);
  expression_ref pi_E = Args.evaluate(1);
  double t = *Args.evaluate_as<Double>(2);

  std::vector<double> pi = get_vector<double,Double>(pi_E);
  
  return object_ptr<const Object>(new MatrixObject( exp(*L, pi, t) ) );
}

expression_ref LExp = lambda_expression( LExp_Op() );

//---------------------------------------------------------------------------------------//

