#include "computation/computation.H"
#include "computation/builtins/conversion.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  return numeric_conversion_function<int,double>(Args);
}
