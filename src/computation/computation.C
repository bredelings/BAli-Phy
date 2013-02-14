#include "computation.H"
#include "expression.H"

int OperationArgs::reg_for_slot(int slot) const
{
  int index = assert_is_a<index_var>(reference(slot))->index;

  return current_closure().lookup_in_env(index);
}

int OperationArgs::n_args() const {return current_closure().exp->sub.size();}

expression_ref OperationArgs::reference(int slot) const {return current_closure().exp->sub[slot];}

const closure& OperationArgs::evaluate_slot_to_closure(int slot)
{
  return evaluate_reg_to_closure(reg_for_slot(slot));
}

int OperationArgs::evaluate_slot_no_record(int slot)
{
  return evaluate_reg_no_record(reg_for_slot(slot));
}

object_ref OperationArgs::evaluate_reg_to_object(int R2)
{
  expression_ref result = evaluate_reg_to_closure(R2).exp->head;
  assert(not is_a<lambda2>(result));
  return result->head;
}

object_ref OperationArgs::evaluate_slot_to_object(int slot)
{
  return evaluate_reg_to_object(reg_for_slot(slot));
}

object_ref OperationArgs::evaluate(int slot)
{
  return evaluate_slot_to_object(slot);
}

