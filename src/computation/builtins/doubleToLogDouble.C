#include "computation/computation.H"
#include "computation/builtins/conversion.H"

extern "C" closure builtin_function_doubleToLogDouble(OperationArgs& Args)
{
  return numeric_conversion_function<double,log_double_t>(Args);
}

