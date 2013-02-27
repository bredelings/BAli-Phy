#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_new_modifiable(OperationArgs& Args)
{
  int token = *Args.evaluate_as<Int>(0);

  reg_heap& M = Args.memory();

  // I think that ownership defaults to the current token, so...
  // ... We wouldn't want the reg to be a modifiable in a token that
  // wasn't marked as an owner of the reg.
  assert( Args.current_token() == token );

  // Get the list of modifiable regs
  pool<int>& modifiable_regs = M.get_modifiable_regs_for_context(token);

  // Determine the index for the new modifiable
  int m_index = modifiable_regs.allocate();

  // Allocate a reg, and fill it with a modifiable of the correct index
  int R1 = Args.allocate(modifiable(m_index));

  // Mark the location of the new modifiable
  modifiable_regs[m_index] = R1;

  // Return a reference to the new modifiable.
  return {index_var(0),{R1}};
}

