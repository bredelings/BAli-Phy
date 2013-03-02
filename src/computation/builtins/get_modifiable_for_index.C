#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_get_modifiable_for_index(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);
  assert( Args.current_token() == token );

  int index = *Args.evaluate_as<Int>(1);

  const reg_heap& M = Args.memory();

  int R1 = M.get_modifiable_regs_for_context(token)[index];

  return {index_var(0),{R1}};
}
