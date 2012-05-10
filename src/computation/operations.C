#include "operations.H"
#include "graph_register.H"
#include "exponential.H"

using std::vector;
using std::string;

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
  object_ptr<const index_var> V = assert_is_a<index_var>(Args.reference(1));
  int arg = Args.current_closure().lookup_in_env( V->index );

  // We could actually change this to a static_cast.  C.exp MUST be an expression.  C.exp[0] MUST be a lambda.
  assert_is_a<lambda2>(C.exp);

  C.exp = C.exp->sub[0];
  C.Env.push_back(arg);
  return C;
}

std::string Apply::name() const {
  return "@";
}

closure Case::operator()(OperationArgs& Args) const
{
  closure C = Args.current_closure();

  closure obj = Args.lazy_evaluate(0);

  int L = (Args.n_args() - 1)/2;

  // FIXME - we really shouldn't be allocating memory here!
  // But this IS easier not to break.
  vector<expression_ref> cases(L);
  vector<expression_ref> bodies(L);
  for(int i=0;i<L;i++)
  {
    cases[i] = Args.reference(1 + 2*i);
    bodies[i] = Args.reference(2 + 2*i);
  }

  assert(not is_a<lambda2>(obj.exp));

  closure result;
  result.Env = C.Env;

  for(int i=0;i<L and not result;i++)
  {
    // If its _, then match it.
    if (object_ptr<const dummy> D2 = is_a<dummy>(cases[i]))
    {
      // We standardize to avoid case x of v -> f(v) so that f cannot reference v.
      assert(D2->index == -1);
      assert(i == L-1);
      
      result.exp = bodies[i];
    }
    else
    {
      // FIXME! Convert every pattern head to an integer...

      // If we are a constructor, then match iff the the head matches.
      if (obj.exp->head->compare(*cases[i]->head))
      {
	if (obj.exp->size())
	{
	  object_ptr<const constructor> C = is_a<constructor>(obj.exp);
	  assert(C);
	  // The number of constructor fields is the same the for case pattern and the case object.
	  assert(obj.exp->size() == C->n_args());
	  // The number of entries in the environment is the same as the number of constructor fields.
	  assert(obj.exp->size() == obj.Env.size());
	}
	
	result.exp = bodies[i];
	
	for(int j=0;j<obj.exp->size();j++)
	{
	  // FIXME! Don't do a dynamic cast here.
	  int index = is_a<index_var>(obj.exp->sub[j])->index;
	  
	  result.Env.push_back( obj.lookup_in_env( index ) );
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

  closure C = Args.current_closure();

  // We can't do negative-sized arrays
  assert(n >= 0);
  // The function should be represented as a heap variable...
  object_ptr<const index_var> V = is_a<index_var>(f);
  int f_reg = C.lookup_in_env(V->index);
  
  object_ptr<expression> exp = new expression(constructor("Array",n));
  exp->sub.resize(n);

  expression_ref apply_E;
  {
    expression_ref fE = index_var(1);
    expression_ref argE = index_var(0);
    apply_E = (fE, argE);
  }

  closure result;
  result.Env.resize(n);
  for(int i=0;i<n;i++)
  {
    // i
    int i_reg = Args.allocate(expression_ref(i));

    // %1 %0 {f,i}
    int apply_reg = Args.allocate({apply_E,{f_reg, i_reg}});

    // change to result.exp <<= index_var(i)
    exp->sub[i] = index_var(n - 1 - i);

    // Add the var to the environment
    result.Env[i] = apply_reg;
  }
  result.exp = exp;
  
  return result;
}

expression_ref mkArray = lambda_expression( MkArray() );

closure GetIndex::operator()(OperationArgs& Args) const
{
  closure C = Args.lazy_evaluate(0);
  int n = *Args.evaluate_as<Int>(1);

  int N = C.exp->size();

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

  expression_ref pi_E = Args.evaluate_structure(1);
  double t = *Args.evaluate_as<Double>(2);

  std::vector<double> pi = get_vector_from_list<double,Double>(pi_E);
  
  return object_ptr<const Object>(new MatrixObject( exp(*L, pi, t) ) );
}

expression_ref LExp = lambda_expression( LExp_Op() );

//---------------------------------------------------------------------------------------//

