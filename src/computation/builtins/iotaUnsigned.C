#include "computation/computation.H"
#include "computation/builtins/iota.H"

extern "C" closure builtin_function_iotaUnsigned(OperationArgs& Args)
{
  return iota_function<unsigned>(Args);
}
