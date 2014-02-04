#include "computation/computation.H"

extern "C" closure builtin_function_reapply(OperationArgs& Args)
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
  if (Args.current_token())
    Args.evaluate_reg_to_object(apply_reg);

  return {index_var(0),{apply_reg}};
}
