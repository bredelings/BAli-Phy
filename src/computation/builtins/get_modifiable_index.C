#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_get_modifiable_index(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R1 = Args.evaluate_slot_to_reg(0);

  const reg_heap& M = Args.memory();

  assert(is_modifiable(M.access(R1).C.exp));

  return Int(R1);
}
