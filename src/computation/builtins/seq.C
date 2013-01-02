#include "computation/computation.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  Args.evaluate_slot_no_record(0);

  int index = assert_is_a<index_var>(Args.reference(1))->index;
  int R = Args.current_closure().lookup_in_env( index);

  return {index_var(0),{R}};
}
