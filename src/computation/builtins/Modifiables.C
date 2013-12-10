#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_new_modifiable(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  int D = Args.reg_for_slot(0);

  // Allocate a reg, and fill it with a modifiable of the correct index
  expression_ref E(new expression(modifiable(),{index_var(0)}));
  closure C{E,{D}};
  int R1 = Args.allocate(std::move(C));
  M.access(R1).changeable = true;

  // Return a reference to the new modifiable.
  return {index_var(0),{R1}};
}

extern "C" closure builtin_function_new_random_modifiable(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  int D = Args.reg_for_slot(0);

  // Allocate a reg, and fill it with a modifiable of the correct index
  expression_ref E(new expression(modifiable(),{index_var(0)}));
  closure C{E,{D}};
  int R1 = Args.allocate(std::move(C));
  M.access(R1).changeable = true;

  M.add_random_modifiable(R1);

  // Return a reference to the new modifiable.
  return {index_var(0),{R1}};
}

extern "C" closure builtin_function_is_changeable(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R1 = Args.evaluate_slot_to_reg(0);

  const reg_heap& M = Args.memory();
  if (M.reg_is_changeable(R1))
    return constructor("Prelude.True",0);
  else
    return constructor("Prelude.False",0);
}

extern "C" closure builtin_function_is_modifiable(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R1 = Args.evaluate_slot_to_reg(0);

  const reg_heap& M = Args.memory();
  if (M.access(R1).C.exp->head->type() == modifiable_type)
    return constructor("Prelude.True",0);
  else
    return constructor("Prelude.False",0);
}

extern "C" closure builtin_function_get_modifiable_for_index(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R1 = *Args.evaluate_as<Int>(0);

  return {index_var(0),{R1}};
}

extern "C" closure builtin_function_get_modifiable_index(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R1 = Args.evaluate_slot_to_reg(0);

  const reg_heap& M = Args.memory();

  assert(is_modifiable(M.access(R1).C.exp));

  return Int(R1);
}

extern "C" closure builtin_function_set_modifiable_value(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);

  int R1 = Args.evaluate_slot_to_reg(1);
  int R2 = Args.evaluate_slot_to_reg(2);

  Args.memory().set_reg_value(R1, {index_var(0),{R2}}, token);

  return constructor("()",0);
}

extern "C" closure builtin_function_get_modifiable_value(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = *Args.evaluate_as<Int>(0);

  int R1 = Args.evaluate_slot_to_reg(1);

  const reg_heap& M = Args.memory();

  assert( M.access(R1).C.exp->head->type() == modifiable_type);
  assert( M.reg_is_changeable(R1) );

  int R2 = Args.memory().computation_for_reg(token,R1).call;

  assert( R2 );

  return {index_var(0),{R2}};
}
