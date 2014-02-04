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

const closure& OperationArgs::evaluate_slot_to_closure_(int slot)
{
  return evaluate_reg_to_closure_(reg_for_slot(slot));
}

int OperationArgs::evaluate_slot_no_record(int slot)
{
  return evaluate_reg_no_record(reg_for_slot(slot));
}

int OperationArgs::evaluate_slot_to_reg(int slot)
{
  return evaluate_reg_to_reg(reg_for_slot(slot));
}

const closure& OperationArgs::evaluate_reg_to_closure(int R2)
{
  int R3 = evaluate_reg_to_reg(R2);
  int t = current_token();
  if (not t and memory().access(R3).type == reg::type_t::changeable)
    throw no_context();
  return memory().access_result_for_reg(t,R3);
}

const closure& OperationArgs::evaluate_reg_to_closure_(int R2)
{
  int R3 = evaluate_reg_no_record(R2);
  int t = current_token();
  if (not t and memory().access(R3).type == reg::type_t::changeable)
    throw no_context();
  return memory().access_result_for_reg(t,R3);
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

const object_ptr<const Object>& OperationArgs::evaluate_reg_to_object_(int R2)
{
  const object_ptr<const Object>& result = evaluate_reg_to_closure_(R2).exp->head;
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

const object_ptr<const Object>& OperationArgs::evaluate_slot_to_object_(int slot)
{
  return evaluate_reg_to_object_(reg_for_slot(slot));
}

const object_ptr<const Object>& OperationArgs::evaluate(int slot)
{
  return evaluate_slot_to_object(slot);
}

const object_ptr<const Object>& OperationArgs::evaluate_(int slot)
{
  return evaluate_slot_to_object_(slot);
}

