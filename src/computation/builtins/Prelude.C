#include "computation/computation.H"

extern "C" closure builtin_function_log(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(log(*x));
}

extern "C" closure builtin_function_sqrt(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(sqrt(*x));
}

extern "C" closure builtin_function_truncate(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);

  return new Double(trunc(*x));
}

extern "C" closure builtin_function_ceiling(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);

  return new Double(ceil(*x));
}

extern "C" closure builtin_function_floor(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(floor(*x));
}


extern "C" closure builtin_function_round(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(round(*x));
}

extern "C" closure builtin_function_vector_from_list(OperationArgs& Args)
{
  object_ptr<OVector> v (new OVector);

  const closure* top = &Args.evaluate_slot_to_closure(0);
  while(top->exp->size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp->size() == 2);

    int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
    int next_reg = top->lookup_in_env( next_index );

    // Add the element to the list.
    v->t.push_back( Args.evaluate_reg_to_object(element_reg) );
    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));

  return v;
}
