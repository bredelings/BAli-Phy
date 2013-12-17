#include "computation/computation.H"

extern "C" closure builtin_function_join(OperationArgs& Args)
{
  Args.evaluate_slot_to_closure(0);
  return Args.evaluate_slot_to_closure(1);
}
