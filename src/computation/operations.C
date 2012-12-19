#include "operations.H"
#include "graph_register.H"
#include "math/exponential.H"
#include "myexception.H"

using std::vector;
using std::string;

// Q: When is there a benefit to preserving the seq operation, instead of just
//    returning a reference to arg #2?
// A: Firstly, the benefit we are talking about is entirely a space benefit.
//    This only occurs when pre-evaluating arg#1 decreases the size of the
//    expression tree stored in memory.
//
//    Now, if we are re-running the computation because arg#2 is changeable,
//    then we might simplify some subtree of arg#1 that was an unevaluated
//    branch the first time around, *if* arg#1 is changeable and arg#2 is
//    changeable.
//
//    However, in order to allow chains of seq to be eliminated, lets make
//    seq x y just evaluate to y, instead of copying y's result.  (The cost
//    of this is that any additional space cost that could be eliminated
//    during a *second* evaluate of x (from a second evaluation of seq x y)
//    will not be eliminated through a seq.
//
//    So, basically, seqs will always be unchangeable, and therefore will
//    always be eliminated in favor of index_var references to y.
//

// NOTE: (a) Both seq and $ need to look up the memory location of one of their arguments.
//       (b) Both seq and $ avoid evaluating the reg at this location. 
// Thus: Every instead of reference(slot) actually looks up the relevant reg, except for
//       the case operation, which instead uses reference to determine the branches.

using boost::dynamic_pointer_cast;

closure Seq::operator()(OperationArgs& Args) const
{
  Args.evaluate_slot_no_record(0);

  int index = assert_is_a<index_var>(Args.reference(1))->index;
  int R = Args.current_closure().lookup_in_env( index);

  return {index_var(0),{R}};
}

closure Print::operator()(OperationArgs& Args) const
{
  object_ptr<const Object> x = Args.evaluate(0);
  
  return closure(object_ref( new String(x->print() ) ));
}

closure Error::operator()(OperationArgs& Args) const
{
  string message = *Args.evaluate_as<String>(0);
  
  throw myexception()<<message;
}

closure Join::operator()(OperationArgs& Args) const
{
  Args.lazy_evaluate(0);
  return Args.lazy_evaluate(1);
}

closure Reapply::operator()(OperationArgs& Args) const
{
  int index1 = assert_is_a<index_var>(Args.reference(0))->index;
  int R1 = Args.current_closure().lookup_in_env( index1 );

  int index2 = assert_is_a<index_var>(Args.reference(1))->index;
  int R2 = Args.current_closure().lookup_in_env( index2 );

  expression_ref apply_E;
  {
    expression_ref fE = index_var(1);
    expression_ref argE = index_var(0);
    apply_E = (fE, argE);
  }

  // %1 %0 {R1,R2}
  int apply_reg = Args.allocate({apply_E,{R1, R2}});

  // FIXME - aren't we trying to eliminate general evaluation of regs that aren't children?  See below:

  // Evaluate the newly create application reg - and depend upon it!
  Args.evaluate_reg(apply_reg);

  return {index_var(0),{apply_reg}};
}

expression_ref seq = lambda_expression( Seq() );

/* To perform a multi-argument apply in the current framework, we
   don't need to worry about adding PAP nodes, since partially applied functions
   are fine.  We just need to remove n levels of lambdas if there are n levels to remove.

   We could in theory recognize when a function is an apply.  If it doesn't have enough arguments
   we could then add some.  However, the simplest thing to do would just be to evaluate it,
   thus applying any arguments that it has.  We can then add ours to the list.

   @ x y1 y2 y3 ... yn
*/

int get_n_lambdas(const expression_ref& E)
{
  if (E->head->type() == lambda2_type)
    return 1 + get_n_lambdas(E->sub[0]);
  else
    return 0;
}

expression_ref peel_n_lambdas(const expression_ref& E, int n)
{
  if (n == 0)
    return E;
  else if (E->head->type() == lambda2_type)
    return peel_n_lambdas(E->sub[0], n-1);
  else
    std::abort();
}
      

closure Apply::operator()(OperationArgs& Args) const
{
  closure C = Args.lazy_evaluate(0);
  int n_args_given = Args.n_args()-1;

  assert_is_a<lambda2>(C.exp);
  int n_args_needed = get_n_lambdas(C.exp);
  assert(n_args_needed >= 1);
  assert(n_args_given >= 1);

  int n_args_applied = std::min(n_args_given, n_args_needed);
  C.exp = peel_n_lambdas(C.exp, n_args_applied);
  for(int i=0;i<n_args_applied;i++)
  {
    object_ptr<const index_var> V = assert_is_a<index_var>(Args.reference(i+1));
    int arg = Args.current_closure().lookup_in_env( V->index );
    C.Env.push_back(arg);
  }

  // 1. We can apply all the args
  if (n_args_given <= n_args_needed)
    return C;

  // 2. We can only apply some of the args
  else
  {
    int new_head_ref = Args.allocate(std::move(C));
    vector<int> Env = {new_head_ref};
    vector<expression_ref> args = {index_var(n_args_given - n_args_needed)};

    for(int i=n_args_needed;i<n_args_given;i++)
    {
      object_ptr<const index_var> V = assert_is_a<index_var>(Args.reference(i+1));
      int arg = Args.current_closure().lookup_in_env( V->index );
      Env.push_back(arg);

      args.push_back(index_var(n_args_given - i -1));
    }
    expression_ref E2 = {Apply(),args};
    return {E2,Env};
  }
}

std::string Apply::name() const {
  return "@";
}

// Multi-argument lambda-calculus reduction rules.
//
// 1.   /\x[1]...x[n].A             =>   /\x[1]...x[n].A
// 2. @ /\x[1]...x[n].A p[1]...p[m] => @ /\x[1]...x[2].A p[1]...p[m]            if 1<=m<n
// 3. @ /\x[1]...x[n].A p[1]...p[m] => A{x[i]->p[i]}                            if 1<=m=n
// 4. @ /\x[1]...x[n].A p[1]...p[m] => let y=A{x[i]->p[i]} in y p[n+1] ... p[m] if 1<=n<m
//
// 5. @ (@ /\x[1]...x[n].A p[1]...p[m]) q[1]...q[o] => @ /\x[1]...x[n].A p[1]...p[m] ++ q[1]...q[o]
//
// 6. @ B p[1]...p[m] => B' [1]...p[m]  (if B => B').

/*

// Goal here: every closure should know the types of its arguments!
closure MultiApply::operator()(OperationArgs& Args) const
{
  // Always start by evaluating the head.
  closure C = Args.lazy_evaluate(0);

  // Determine the arity of the lambda.
  int lambda_arity = assert_is_a<multi_lambda>(C.exp)->n_args;

  // 2. Not enough arguments evaluates to a partial application
  if (is_a<multi_lambda>(C.exp))
  {
    if (n_args < lambda_arity)
    {
      C.exp = PAP(f,args);
    }
    // 3. Exactly enough arguments executes the code... that returns a closure?  Or what?
    else if (n_args == lambda_arity)
    {
      C = ; // execute the function code.
      // ??? But this needs to call back into the interpreter... right?
    }
    // 4. Too many arguments: let y = @ f p[1]...p[n] in @ y p[n+1] ... p[m]
    else
    {
    }
  }
  // 5. Partial application applied to more arguments.
  else if (is_a<pap>(C.exp))
  {
    
  }

  object_ptr<const index_var> V = assert_is_a<index_var>(Args.reference(1));
  int arg = Args.current_closure().lookup_in_env( V->index );


  C.exp = C.exp->sub[0];
  C.Env.push_back(arg);
  return C;
}

std::string MultiApply::name() const {
  return "@";
}

*/

closure Case::operator()(OperationArgs& Args) const
{
  // Resizing of the memory can occur here, invalidating previously computed pointers
  // to closures.  The *index* within the memory shouldn't change, though.
  const closure& obj = Args.lazy_evaluate(0);

  // Therefore, we must compute this *after* we do the computation above, since
  // we're going to hold on to it.  Otherwise the held reference would become
  // *invalid* after the call above!
  const closure& C = Args.current_closure();

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
#ifndef NDEBUG
	if (obj.exp->size())
	{
	  object_ptr<const constructor> C = assert_is_a<constructor>(obj.exp);
	  // The number of constructor fields is the same the for case pattern and the case object.
	  assert(obj.exp->size() == C->n_args());
	  // The number of entries in the environment is the same as the number of constructor fields.
	  assert(obj.exp->size() == obj.Env.size());
	}
#endif	
	result.exp = bodies[i];
	
	for(int j=0;j<obj.exp->size();j++)
	{
	  // FIXME! Don't do a dynamic cast here.
	  int index = assert_is_a<index_var>(obj.exp->sub[j])->index;
	  
	  result.Env.push_back( obj.lookup_in_env( index ) );
	}
      }
    }
  }

  if (not result)
    throw myexception()<<"Case: object doesn't match any alternative in '"<<make_case_expression(obj.exp, cases, bodies)<<"'";

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

  const closure& C = Args.current_closure();

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
  const closure& C = Args.lazy_evaluate(0);
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

  //FIXME! We shouldn't be calling graph_normalize here.

  return graph_normalize(Tuple(0,N-1));
}

expression_ref bounds = lambda_expression( ArrayBounds() );

closure LExp_Op::operator()(OperationArgs& Args) const
{
  const EigenValues& L = *Args.evaluate_as<EigenValues>(0);
  const Vector<double>& pi = *Args.evaluate_as< Vector<double> >(1);
  double t = *Args.evaluate_as<Double>(2);

  Matrix E = exp(L, pi, t);
  MatrixObject* M = new MatrixObject;
  M->t.assign_temporary(E);
  return M;
}

expression_ref LExp = lambda_expression( LExp_Op() );

closure Negate::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
    return -(*xd);
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
    return -(*xi);
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
    return -(*xld);
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
    return -(*xc);
  else
    throw myexception()<<"Negate: object '"<<x->print()<<"' is not Double, Int, Log_Double, or Char'";
}

closure Add::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return (*xd) + (*yd);
  }
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return (*xi) + (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return (*xld) + (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return (*xc) + (*yc);
  }
  else
    throw myexception()<<"Add: object '"<<x->print()<<"' is not Double, Int, Log_Double, or Char'";
}

closure Minus::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return (*xd) - (*yd);
  }
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return (*xi) - (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return (*xld) - (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return (*xc) - (*yc);
  }
  else
    throw myexception()<<"Minus: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

closure Divide::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return (*xd) / (*yd);
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return (*xi) / (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return (*xld) / (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return (*xc) / (*yc);
  }
  else
    throw myexception()<<"Divide: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

closure Multiply::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return (*xd) * (*yd);
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return (*xi) * (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return (*xld) * (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return (*xc) * (*yc);
  }
  else
    throw myexception()<<"Multiply: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

closure LessThan::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) < (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) < (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) < (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) < (*yc)};
  }
  else
    throw myexception()<<"LessThan: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

closure Equals::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) == (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) == (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) == (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) == (*yc)};
  }
  else
    throw myexception()<<"Equals: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

closure NotEquals::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) != (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) != (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) != (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) != (*yc)};
  }
  else
    throw myexception()<<"NotEquals: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

closure GreaterThan::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) > (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) > (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) > (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) > (*yc)};
  }
  else
    throw myexception()<<"GreaterThan: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

//---------------------------------------------------------------------------------------//

closure Mod::operator()(OperationArgs& Args) const
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return Int( *xi % *yi );
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return Char( *xc % *yc );
  }
  else
    throw myexception()<<"Mod: object '"<<x->print()<<"' is not Int, or Char'";
}

//---------------------------------------------------------------------------------------//

tribool Log_Op::compare(const Object& o) const 
{
  return dynamic_cast<const Log_Op*>(&o);
}

closure Log_Op::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(log(*x));
}

const expression_ref Log = lambda_expression( Log_Op() );

//---------------------------------------------------------------------------------------//

tribool Sqrt_Op::compare(const Object& o) const 
{
  return dynamic_cast<const Sqrt_Op*>(&o);
}

closure Sqrt_Op::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x >= 0.0);

  return new Double(sqrt(*x));
}

const expression_ref Sqrt = lambda_expression( Sqrt_Op() );

//---------------------------------------------------------------------------------------//

tribool Exp_Op::compare(const Object& o) const 
{
  return dynamic_cast<const Exp_Op*>(&o);
}

closure Exp_Op::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);

  return new Double(exp(x));
}

//---------------------------------------------------------------------------------------//

/*
 * Note that does doesn't work if the structure being evaluated contains
 * expressions that *evaluate to* parameters.  This is true even if the
 * structures have reg_vars (or reg_var chains) pointing to parameters.
 * The expressions would have to *actually contain* parameters.
 *
 * The idea here is that the only expressions that we would consider would be
 * constructors with parameters as fields.
 *
 * This is somewhat problematic, since *arrays* would probably not actually
 * have parameters as fields.  For example, if a list contains parameters as
 * fields, the listArray' constructed from it would probably not!
 */

tribool Get_Address::compare(const Object& o) const 
{
  return dynamic_cast<const Get_Address*>(&o);
}

closure Get_Address::operator()(OperationArgs& Args) const
{
  object_ptr<const index_var> V = assert_is_a<index_var>(Args.reference(1));
  int arg = Args.current_closure().lookup_in_env( V->index );

  return new Int(arg);
}

//---------------------------------------------------------------------------------------//

def_binary_operator2(-,Minus)
def_binary_operator2(+,Add)
def_binary_operator2(*,Multiply)
def_binary_operator2(/,Divide)
def_binary_operator2(<,LessThan)
def_binary_operator2(>,GreaterThan)
// These operators should check equality of memory addresses, not construct a new expression_ref!
//def_binary_operator2(==,Equals)
//def_binary_operator2(!=,NotEquals)
#undef def_binary_operator2
