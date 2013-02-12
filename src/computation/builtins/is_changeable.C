#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_is_changeable(OperationArgs& Args)
{
  int token = *Args.evaluate_as<Int>(0);
  int R1 = Args.evaluate_slot_(1);

  const reg_heap& M = Args.memory();
  if (M.access(R1).changeable)
    return constructor("Prelude.True",0);
  else
    return constructor("Prelude.False",0);
}
