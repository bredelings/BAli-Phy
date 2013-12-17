#include "computation/computation.H"
#include "iota.H"

extern "C" closure builtin_function_iotaUnsigned(OperationArgs& Args)
{
  return iota_function<unsigned>(Args);
}
