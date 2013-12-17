#include "computation/computation.H"
#include "conversion.H"

extern "C" closure builtin_function_intToDouble(OperationArgs& Args)
{
  return numeric_conversion_function<int,double>(Args);
}
