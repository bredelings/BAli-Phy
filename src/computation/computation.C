#include "computation.H"
#include "expression.H"
#include "graph_register.H"

int OperationArgs::reg_for_slot(int slot) const
{
  int index = as_<index_var>(reference(slot)).index;

  return current_closure().lookup_in_env(index);
}

int OperationArgs::n_args() const {return current_closure().exp.size();}

const expression_ref& OperationArgs::reference(int slot) const {return current_closure().exp.sub()[slot];}

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

const expression_ref& OperationArgs::evaluate_reg_to_object(int R2)
{
  const expression_ref& result = evaluate_reg_to_closure(R2).exp;
#ifndef NDEBUG
  if (is_a<lambda2>(expression_ref(result)))
    throw myexception()<<"Evaluating lambda as object: "<<result.print();
#endif
  return result;
}

const expression_ref& OperationArgs::evaluate_reg_to_object_(int R2)
{
  const expression_ref& result = evaluate_reg_to_closure_(R2).exp;
#ifndef NDEBUG
  if (is_a<lambda2>(expression_ref(result)))
    throw myexception()<<"Evaluating lambda as object: "<<result.print();
#endif
  return result;
}

const expression_ref& OperationArgs::evaluate_slot_to_object(int slot)
{
  return evaluate_reg_to_object(reg_for_slot(slot));
}

const expression_ref& OperationArgs::evaluate_slot_to_object_(int slot)
{
  return evaluate_reg_to_object_(reg_for_slot(slot));
}

const expression_ref& OperationArgs::evaluate(int slot)
{
  return evaluate_slot_to_object(slot);
}

const expression_ref& OperationArgs::evaluate_(int slot)
{
  return evaluate_slot_to_object_(slot);
}

int OperationArgs::allocate(closure&& C)
{
  if (C.exp.head().ptr()->type() == index_var_type)
  {
    int index = C.exp.as_<index_var>().index;

    int r = C.lookup_in_env( index );
    
    assert(M.is_used(r));

    return r;
  }

  int r = M.push_temp_head();
  M.set_C(r, std::move(C) );
  n_allocated++;
  return r;
}

OperationArgs::OperationArgs(reg_heap& m)
  :M(m)
{ }

OperationArgs::~OperationArgs()
{
  for(int i=0;i<n_allocated;i++)
    M.pop_temp_head();
}
