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
//
// Note that the @ in rules #2 and #5 is a PAP (partial application).

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
  expression_ref E2 = E;
  int n = 0;
  while(E2->head->type() == lambda2_type)
  {
    E2 = E2->sub[0];
    n++;
  }
  return n;
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
  closure C = Args.evaluate_slot_to_closure(0);
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

closure Case::operator()(OperationArgs& Args) const
{
  // Resizing of the memory can occur here, invalidating previously computed pointers
  // to closures.  The *index* within the memory shouldn't change, though.
  const closure& obj = Args.evaluate_slot_to_closure(0);

  // Therefore, we must compute this *after* we do the computation above, since
  // we're going to hold on to it.  Otherwise the held reference would become
  // *invalid* after the call above!
  const closure& C = Args.current_closure();

  int L = (Args.n_args() - 1)/2;

#ifndef NEDBUG
  vector<expression_ref> cases(L);
  vector<expression_ref> bodies(L);
  for(int i=0;i<L;i++)
  {
    cases[i] = Args.reference(1 + 2*i);
    bodies[i] = Args.reference(2 + 2*i);
  }

  if (is_a<lambda2>(obj.exp))
    throw myexception()<<"Case argument is a lambda '"<<make_case_expression(obj.exp, cases, bodies)<<"'";
#endif

  closure result;
  result.Env = C.Env;

  for(int i=0;i<L and not result;i++)
  {
    const expression_ref& this_case = Args.reference(1 + 2*i);
    const expression_ref& this_body = Args.reference(2 + 2*i);

    // If its _, then match it.
    if (this_case->head->type() == dummy_type)
    {
      // We standardize to avoid case x of v -> f(v) so that f cannot reference v.
      assert(is_wildcard(this_case));
      assert(i == L-1);
      
      result.exp = this_body;
    }
    else
    {
      // FIXME! Convert every pattern head to an integer...

      // If we are a constructor, then match iff the the head matches.
      if (obj.exp->head->compare(*this_case->head))
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
	result.exp = this_body;
	
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
#ifdef NDEBUG
    throw myexception()<<"Case: object doesn't match any alternative";
#else
    throw myexception()<<"Case: object doesn't match any alternative in '"<<make_case_expression(obj.exp, cases, bodies)<<"'";
#endif

  result = get_trimmed(result);

  return result;
}

std::string Case::name() const {
  return "case";
}

