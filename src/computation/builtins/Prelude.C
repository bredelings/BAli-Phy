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
