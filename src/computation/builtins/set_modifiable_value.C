#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_set_modifiable_value(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);
  assert( Args.current_token() == token );

  int R1 = Args.evaluate_slot_to_reg(1);
  int R2 = Args.evaluate_slot_to_reg(2);

  Args.memory().set_reg_value(R1, {index_var(0),{R2}}, token);

  return constructor("()",0);
}
