#include "likelihood.H"
#include "logsum.H"
#include "substitution.H"
#include "setup.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "probability.H"
#include "alignment-util.H"
#include "util.H"

efloat_t topology_weight(const Parameters& P, const SequenceTree& T) 
{
  efloat_t p = 1;
  
  for(int i=0;i<P.partitions.size();i++) {
    if (implies(T,P.partitions[i])) {
      //      std::cerr<<"found!  "<<P.partitions[i]<<std::endl;
      p *= P.partition_weights[i];
    }
    else
      ;//      std::cerr<<"not found!  "<<P.partitions[i]<<std::endl;
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

/// Hyper-prior + Tree prior + SModel prior + IModel prior
efloat_t prior(const Parameters& P) 
{
  efloat_t p = 1;

  const double branch_mean_mean = loadvalue(P.keys,"branch_mean_mean",1.0);

  // prior on mu, the mean branch length
  p *= exponential_pdf(P.branch_mean(), branch_mean_mean);

  // prior on the topology and branch lengths
  p *= prior(P, P.T, P.branch_mean());

  // prior on the substitution model
  p *= P.SModel().prior();

  // prior on the insertion/deletion model
  if (P.has_IModel()) 
  {
    p *= P.IModel().prior();

    const double p_unaligned = loadvalue(P.keys,"P_aligned",0.0);

    efloat_t pNA = p_unaligned;

    efloat_t pA = (1.0 - p_unaligned);

    for(int b=0;b<P.T.n_branches();b++)
      if (not P.branch_HMM_type[b])
	p *= pA;
      else
	p *= pNA;
  }

  return p;
}

/// Probability of a pairwise alignment
efloat_t prior_branch(const alignment& A,const indel::PairHMM& Q,int target,int source) {
  vector<int> state = get_path(A,target,source);

  efloat_t P = Q.start(state[0]);
  for(int i=1;i<state.size();i++) 
    P *= Q(state[i-1],state[i]);
  
  return P;
}

/// Probability of a multiple alignment if branch alignments independant
efloat_t prior_HMM_nogiven(const alignment& A,const Parameters& P) 
{
  const Tree& T = P.T;

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


efloat_t prior_HMM_rootless_scale(const alignment& A, const Parameters& P)
{
  const Tree& T = P.T;

#ifndef NDEBUG
  assert(P.has_IModel());
  check_internal_nodes_connected(A,T);
#endif
  
  efloat_t Pr = 1;

  for(int i=T.n_leaves();i<T.n_nodes();i++) {
    int l = A.seqlength(i);
    Pr /= P.IModel().lengthp(l);
    Pr /= P.IModel().lengthp(l);
  }

  return Pr;
}

//NOTE  - this will have to change if we ever have sequences at internal nodes
//        that have other than 3 neighbors
efloat_t prior_HMM(const alignment& A,const Parameters& P) 
{
  return prior_HMM_nogiven(A,P) * prior_HMM_rootless_scale(A,P);
}
