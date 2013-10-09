#include "computation.H"
#include "expression.H"
#include "graph_register.H"

int OperationArgs::reg_for_slot(int slot) const
{
  int index = assert_is_a<index_var>(reference(slot))->index;

  return current_closure().lookup_in_env(index);
}

int OperationArgs::n_args() const {return current_closure().exp->sub.size();}

const expression_ref& OperationArgs::reference(int slot) const {return current_closure().exp->sub[slot];}

const closure& OperationArgs::evaluate_slot_to_closure(int slot)
{
  return evaluate_reg_to_closure(reg_for_slot(slot));
}

int OperationArgs::evaluate_reg_no_record(int R)
{
  return evaluate_reg_no_record(R, evaluate_changeables());
}

int OperationArgs::evaluate_slot_no_record(int slot, bool ec)
{
  return evaluate_reg_no_record(reg_for_slot(slot), ec);
}

int OperationArgs::evaluate_slot_no_record(int slot)
{
  return evaluate_slot_no_record(slot, evaluate_changeables());
}

int OperationArgs::evaluate_reg_to_reg(int R)
{
  return evaluate_reg_to_reg(R, evaluate_changeables());
}

int OperationArgs::evaluate_slot_to_reg(int slot, bool ec)
{
  return evaluate_reg_to_reg(reg_for_slot(slot), ec);
}

int OperationArgs::evaluate_slot_to_reg(int slot)
{
  return evaluate_slot_to_reg(slot, evaluate_changeables());
}

/// Evaluate the reg R2, record dependencies, and return the result.
const closure& OperationArgs::evaluate_reg_to_closure(int R2, bool ec)
{
  int R3 = evaluate_reg_to_reg(R2, ec);
  if (ec)
    return memory().access_result_for_reg(current_token(),R3);
  else
    return memory().access(R3).C;
}

const closure& OperationArgs::evaluate_reg_to_closure(int R)
{
  return evaluate_reg_to_closure(R, evaluate_changeables());
}

const object_ptr<const Object>& OperationArgs::evaluate_reg_to_object(int R2)
{
  const object_ptr<const Object>& result = evaluate_reg_to_closure(R2).exp->head;
#ifndef NDEBUG
  if (is_a<lambda2>(expression_ref(result)))
    throw myexception()<<"Evaluating lambda as object: "<<result->print();
#endif
  return result;
}

const object_ptr<const Object>& OperationArgs::evaluate_slot_to_object(int slot)
{
  return evaluate_reg_to_object(reg_for_slot(slot));
}

const object_ptr<const Object>& OperationArgs::evaluate(int slot)
{
  return evaluate_slot_to_object(slot);
}

