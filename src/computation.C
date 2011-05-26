#include "computation.H"
#include "context.H"
#include "formula.H"

ContextOperationArgs::ContextOperationArgs(Context& A, int i)
  :CTX(A), index_of_caller(i) 
{ 
  int n_input_slots = CTX.F->n_input_indices(index_of_caller);
  
  computation = boost::shared_ptr<Computation>( new Computation(n_input_slots) );
}

boost::shared_ptr<const Object> ContextOperationArgs::evaluate(int slot)
{
  int index_to_evaluate = CTX.F->input_indices(index_of_caller)[slot];
  if (not computation->used_values[slot])
  {
    computation->used_values[slot] = CTX.evaluate(index_to_evaluate);
    computation->slots_used_order.push_back(slot);
  }
  
  // Whatever we evaluated should NOT evaluate to NULL!
  assert( computation->used_values[slot] );
  
  return computation->used_values[slot];
}

