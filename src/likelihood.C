#include "likelihood.H"
#include "logsum.H"
#include "substitution.H"
#include "setup.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "probability.H"
#include "alignment-util.H"
#include "util.H"
#include "2way.H"

efloat_t topology_weight(const Parameters& P, const SequenceTree& T) 
{
  efloat_t p = 1;
  
  for(int i=0;i<P.partitions.size();i++) {
    if (implies(T,P.partitions[i])) {
      //      std::cerr<<"found!  "<<P.partitions[i]<<std::endl;
      p *= P.partition_weights[i];
    }
    else
      { } //      std::cerr<<"not found!  "<<P.partitions[i]<<std::endl;
  }
  //  std::cerr<<"total_weight = "<<p<<std::endl;

  return p;
}


/// Tree prior: branch lengths & topology
efloat_t prior(const SequenceTree& T,double branch_mean) 
{
  efloat_t p = 1;

  // --------- uniform prior on topologies --------//
  if (T.n_leaves()>3)
    p /= num_topologies(T.n_leaves());

  // ---- Exponential prior on branch lengths ---- //
  for(int i=0;i<T.n_branches();i++) 
    p *= exponential_pdf(T.branch(i).length(), branch_mean);

  return p;
}

/// Tree prior: branch lengths & topology
efloat_t prior(const Parameters& P, const SequenceTree& T,double branch_mean) 
{
  efloat_t p = prior(T,branch_mean);

  p *= topology_weight(P,T);

  return p;
}

ublas::matrix<int> get_path_counts(const alignment& A,int node1, int node2) 
{
  using namespace A2;

  int state1 = states::S;

  ublas::matrix<int> counts(5,5);
  counts.clear();

  for(int column=0;column<A.length();column++) 
  {
    int state2 = -1;
    if (A.gap(column,node1)) {
      if (A.gap(column,node2)) 
       continue;
      else
       state2 = states::G1;
    }
    else {
      if (A.gap(column,node2))
       state2 = states::G2;
      else
       state2 = states::M;
    }

    counts(state1,state2)++;
    state1 = state2;
  }

  counts(state1,states::E)++;

  return counts;
}

/// Probability of a pairwise alignment
efloat_t prior_branch_from_counts(const ublas::matrix<int>& counts,const indel::PairHMM& Q)
{
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
      if (counts(i,j))
	P *= pow<efloat_t>(Qij,counts(i,j));
    }
  
  // Account for ?-E end probability
  if (not counts(states::S,states::E))
    for(int i=0;i<Q.size1();i++)
      if (counts(i,states::E))
       P *= Q(i,states::E);

  return P;
}

/// Probability of a pairwise alignment
efloat_t prior_branch(const alignment& A,const indel::PairHMM& Q,int target,
int source) 
{
  ublas::matrix<int> counts = get_path_counts(A,target,source);

  return prior_branch_from_counts(counts,Q);
}

/// Probability of a multiple alignment if branch alignments independant
efloat_t prior_HMM_nogiven(const data_partition& P) 
{
  const alignment& A = *P.A;
  const Tree& T = *P.T;

#ifndef NDEBUG
  assert(P.has_IModel());
  check_internal_nodes_connected(A,T);
#endif
  
  efloat_t Pr = 1;

  for(int b=0;b<T.n_branches();b++) {
    int target = T.branch(b).target();
    int source  = T.branch(b).source();
    Pr *= prior_branch(A, P.branch_HMMs[b], target, source);
  }
  
  return Pr;
}


efloat_t prior_HMM_rootless_scale(const data_partition& P)
{
  const alignment& A = *P.A;
  const Tree& T = *P.T;

#ifndef NDEBUG
  assert(P.has_IModel());
  check_internal_nodes_connected(A,T);
#endif
  
  efloat_t Pr = 1;

  for(int i=T.n_leaves();i<T.n_nodes();i++) {
    int l = P.seqlength(i);
    efloat_t temp = P.IModel().lengthp(l);
    Pr /= (temp*temp);
  }

  return Pr;
}

//NOTE  - this will have to change if we ever have sequences at internal nodes
//        that have other than 3 neighbors
efloat_t prior_HMM(const data_partition& P) 
{
  return prior_HMM_nogiven(P) * prior_HMM_rootless_scale(P);
}
