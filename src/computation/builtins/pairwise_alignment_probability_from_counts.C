#include "computation/computation.H"
#include "dp/2way.H"
#include "imodel/imodel.H"

extern "C" closure builtin_function_pairwise_alignment_probability_from_counts(OperationArgs& Args)
{
  const ublas::matrix<int>& counts = Args.evaluate_as<Box<ublas::matrix<int>>>(0)->t;
  const indel::PairHMM& Q = *Args.evaluate_as<indel::PairHMM>(1);

  using namespace A2;

  efloat_t P=1;

  // Account for S-? start probability
  for(int i=0;i<Q.size2();i++)
    if (counts(states::S,i))
      P *= Q.start(i);

  // Account for the mass of transitions
  for(int i=0;i<3;i++)
    for(int j=0;j<3;j++) {
      efloat_t Qij = Q(i,j);
      // FIXME - if we propose really bad indel parameters, we can get log(Q_ij) where Qij == 0
      if (counts(i,j))
	P *= pow(Qij,counts(i,j));
    }
  
  // Account for ?-E end probability
  if (not counts(states::S,states::E))
    for(int i=0;i<Q.size1();i++)
      if (counts(i,states::E))
       P *= Q(i,states::E);

  return Log_Double(P);  
}
