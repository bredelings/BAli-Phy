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

