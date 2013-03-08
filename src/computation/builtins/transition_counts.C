#include "computation/computation.H"
#include "dp/2way.H"

extern "C" closure builtin_function_transition_counts(OperationArgs& Args)
{
  const pairwise_alignment_t& A = *Args.evaluate_as<pairwise_alignment_t>(0);

  Box<ublas::matrix<int>> result;
  ublas::matrix<int>& counts = result.t;
  counts.resize(5,5);
  counts.clear();

  using namespace A2;

  for(int column=1;column<A.size();column++) 
    counts(A[column-1],A[column])++;

  return result;
}
