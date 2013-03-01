#include "computation.H"
#include "expression.H"
#include "graph_register.H"

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
    return memory().access_result(R3);
  else
    return memory().access(R3).C;
}

const closure& OperationArgs::evaluate_reg_to_closure(int R)
{
  return evaluate_reg_to_closure(R, evaluate_changeables());
}

object_ref OperationArgs::evaluate_reg_to_object(int R2)
{
  expression_ref result = evaluate_reg_to_closure(R2).exp->head;
#ifndef NDEBUG
  if (is_a<lambda2>(result))
    throw myexception()<<"Evaluating lambda as object: "<<result->print();
#endif
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

