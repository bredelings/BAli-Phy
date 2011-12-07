#include "computation.H"
#include "context.H"
#include "formula.H"

ContextOperationArgs::ContextOperationArgs(const Context& A, int i)
  :CTX(A), index_of_caller(i) 
{ 
  int n_input_slots = CTX.get_formula()->n_input_indices(index_of_caller);
  
  computation = boost::shared_ptr<Computation>( new Computation(n_input_slots) );
}

boost::shared_ptr<const Object> ContextOperationArgs::reference(int slot) const
{
  std::abort();

  int index_to_evaluate = CTX.get_formula()->input_indices(index_of_caller)[slot];
  return CTX.get_sub_expression(index_to_evaluate);
}

boost::shared_ptr<const Object> ContextOperationArgs::evaluate(int slot)
{
  int index_to_evaluate = CTX.get_formula()->input_indices(index_of_caller)[slot];
  if (not computation->used_values[slot])
  {
    computation->used_values[slot] = CTX.evaluate(index_to_evaluate);
    computation->slots_used_order.push_back(slot);
  }
  
  // Whatever we evaluated should NOT evaluate to NULL!
  assert( computation->used_values[slot] );
  
  return computation->used_values[slot];
}

