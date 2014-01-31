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
  M.make_reg_changeable(R1);

  // Return a reference to the new modifiable.
  return {index_var(0),{R1}};
}

// This could also take an initial value -- that value would need to not depend
// on anything.

extern "C" closure builtin_function_new_random_modifiable(OperationArgs& Args)
{
  //  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  int D = Args.reg_for_slot(0);

  int V = Args.reg_for_slot(1);

  // Allocate a reg, and fill it with a modifiable of the correct index
  expression_ref E(new expression(modifiable(),{index_var(0)}));
  closure C{E,{D}};
  int r = Args.allocate(std::move(C));
  M.make_reg_changeable(r);
  M.set_shared_value(r,V);

  M.add_random_modifiable(r);

  // Return a reference to the new modifiable.
  return {index_var(0),{r}};
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

  int c = *Args.evaluate_as<Int>(0);

  int R1 = Args.evaluate_slot_to_reg(1);
  int R2 = Args.evaluate_slot_to_reg(2);

  Args.memory().set_reg_value_in_context(R1, {index_var(0),{R2}}, c);

  return constructor("()",0);
}

extern "C" closure builtin_function_get_modifiable_value(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int c = *Args.evaluate_as<Int>(0);

  int R1 = Args.evaluate_slot_to_reg(1);

  int R2 = Args.memory().get_modifiable_value_in_context(R1, c);

  assert( R2 );

  return {index_var(0),{R2}};
}

extern "C" closure builtin_function_add_parameter(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  const std::string& name = *Args.evaluate_as<String>(0);

  int R = Args.evaluate_slot_to_reg(1);

  auto& M = Args.memory();

  M.parameters.push_back({name,R});

  return constructor("()",0);
}

extern "C" closure builtin_function_register_probability(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R = Args.reg_for_slot(0);

  auto& M = Args.memory();

  M.probability_heads.push_back(R);

  return constructor("()",0);
}
