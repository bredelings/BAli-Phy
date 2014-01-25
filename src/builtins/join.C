#include "computation/computation.H"

extern "C" closure builtin_function_join(OperationArgs& Args)
{
  Args.evaluate_slot_to_reg(0);
  int R = Args.evaluate_slot_to_reg(1);

  return {index_var(0),{R}};
}
