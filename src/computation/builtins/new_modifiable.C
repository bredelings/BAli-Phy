#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_new_modifiable(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  // Allocate a reg, and fill it with a modifiable of the correct index
  int R1 = Args.allocate(modifiable());
  M.access(R1).changeable = true;

  M.add_random_modifiable(R1);

  // Return a reference to the new modifiable.
  return {index_var(0),{R1}};
}

