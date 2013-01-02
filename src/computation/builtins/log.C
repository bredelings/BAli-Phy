#include "computation.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(log(*x));
}
