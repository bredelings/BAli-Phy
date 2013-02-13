#include "computation/computation.H"

extern "C" closure builtin_function_getIndex(OperationArgs& Args)
{
  const closure& C = Args.evaluate_slot_to_closure(0);
  int n = *Args.evaluate_as<Int>(1);

  int N = C.exp->size();

  if (n < 0 or n >= N)
    throw myexception()<<"Trying to access index "<<n<<" in array of size "<<N<<".";
      
  // Return a reference to the heap variable pointed to by the nth entry
  return {index_var(0), {C.Env[n]} };
}
