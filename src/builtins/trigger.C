#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_trigger(OperationArgs& Args)
{
  int i = *Args.evaluate_as<Int>(0);

  reg_heap& M = Args.memory();

  int token = Args.current_token();

  M.triggers(token).push_back(i);

  return constructor("()",0);
}
