#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_get_modifiable_index(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);
  assert( Args.current_token() == token );

  int R1 = Args.evaluate_slot_to_reg(1);

  const reg_heap& M = Args.memory();

  int index = is_a<modifiable>(M.access(R1).C.exp)->index;

  return Int(index);
}
