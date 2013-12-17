#include "computation/computation.H"

extern "C" closure builtin_function_exp(OperationArgs& Args)
{
  double x = *Args.evaluate_as<Double>(0);

  return new Double(exp(x));
}
