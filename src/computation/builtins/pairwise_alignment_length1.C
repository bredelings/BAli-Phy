#include "computation/computation.H"
#include "dp/2way.H"

extern "C" closure builtin_function_pairwise_alignment_length1(OperationArgs& Args)
{
  return Int(Args.evaluate_as<pairwise_alignment_t>(0)->length1());
}
