#include "computation/computation.H"

extern "C" closure builtin_function_arraySize(OperationArgs& Args)
{
  object_ptr<const expression> A = convert<const expression>( Args.evaluate_slot_to_closure(0).exp );
  int N = A->sub.size()-1;

  return Int(N);
}
