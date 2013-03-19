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
