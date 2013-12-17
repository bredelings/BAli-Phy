#include "computation/computation.H"

extern "C" closure builtin_function_getIndex(OperationArgs& Args)
{
  int n = *Args.evaluate_as<Int>(1);
  // Do this second, so that evaluation of the 1st argument can't call expand_memory afterwards.
  const closure& C = Args.evaluate_slot_to_closure(0);

  int N = C.exp->size();
  assert(C.Env.size() == C.exp->size());

  if (n < 0 or n >= N)
    throw myexception()<<"Trying to access index "<<n<<" in array of size "<<N<<".";
      
  // Return a reference to the heap variable pointed to by the nth entry
  return {index_var(0), {C.Env[n]} };
}
