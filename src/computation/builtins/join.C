#include "computation/computation.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  Args.lazy_evaluate(0);
  return Args.lazy_evaluate(1);
}
