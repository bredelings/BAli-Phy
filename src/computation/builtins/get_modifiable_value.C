#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_get_modifiable_value(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);
  assert( Args.current_token() == token );

  int R1 = Args.evaluate_slot_to_reg(1);

  const reg_heap& M = Args.memory();

  assert( M.access(R1).C.exp->head->type() == modifiable_type);
  assert( M.access(R1).changeable );

  int R2 = Args.memory().access(R1).call;

  assert( R2 );

  return {index_var(0),{R2}};
}
