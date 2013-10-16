#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_is_changeable(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);
  assert( Args.current_token() == token );

  int R1 = Args.evaluate_slot_to_reg(1);

  const reg_heap& M = Args.memory();
  if (M.reg_is_changeable(R1))
    return constructor("Prelude.True",0);
  else
    return constructor("Prelude.False",0);
}
