#include "likelihood.H"
#include "logsum.H"
#include "substitution.H"
#include "setup.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "probability.H"
#include "alignment-util.H"

double prior3(const alignment& A,const Parameters& P) {
  return prior_HMM(A,P) + prior(P);
}

double likelihood3(const alignment& A,const Parameters& P) {
  return substitution::Pr(A,P); // also deals w/ frequencies
}

double probability3(const alignment& A,const Parameters& P) {
  return likelihood3(A,P) + prior3(A,P);
}


/// FIXME if X ~ Exp(mu), then g(y) = exp(y-exp(y)/mu)/mu
///                         ln g(y) = log(mu) + (y-exp(y))/mu 


/// Tree prior: branch lengths & topology
double prior(const SequenceTree& T,double branch_mean) {
  double p = 0;

  /* ----- 1/(number of topologies) -----*/
  if (T.n_leaves()>3)
    p = -log_num_topologies(T.n_leaves());

  /* ---- PROD_i exp(- T[i] / mu )/ mu ---- */
  for(int i=0;i<T.n_branches();i++) 
    p += (-log(branch_mean) - T.branch(i).length()/branch_mean );
  return p;
}

/// Hyper-prior + Tree prior + SModel prior + IModel prior
double prior(const Parameters& P) {
  double p = 0;

  const double branch_mean_mean = 0.04;

  // prior on the mu, the mean branch length
  p += exp_exponential_pdf(log(P.branch_mean),branch_mean_mean);

  // prior on the topology and branch lengths
  p += prior(P.T, P.branch_mean);

  // prior on the substitution model
  p += P.SModel().prior();

  // prior on the insertion/deletion model
  p += P.IModel().prior(); // prior(branch_mean)?

  return p;
}

/// Probability of a pairwise alignment
double prior_branch(const alignment& A,const indel::PairHMM& Q,int target,int source) {
  vector<int> state = get_path(A,target,source);

  efloat_t P = Q.start(state[0]);
  for(int i=1;i<state.size();i++) 
    P *= Q(state[i-1],state[i]);
  
  return log(P);
}

/// Probability of a multiple alignment if branch alignments independant
double prior_HMM_nogiven(const alignment& A,const Parameters& P) {
  const Tree& T = P.T;

  double Pr = 0;

#ifndef NDEBUG
  check_internal_nodes_connected(A,P.T);
#endif
  
  for(int b=0;b<T.n_branches();b++) {
    int target = T.branch(b).target();
    int source  = T.branch(b).source();
    double p = prior_branch(A, P.branch_HMMs[b], target,source);
    Pr += p;
  }
  
  return Pr;
}

//NOTE  - this will have to change if we ever have sequences at internal nodes
//        that have other than 3 neighbors
double prior_HMM(const alignment& A,const Parameters& P) {
  const Tree& T = P.T;

#ifndef NDEBUG
  check_internal_nodes_connected(A,P.T);
#endif

  double Pr = 0;
  for(int b=0;b<T.n_branches();b++) {
    int target = T.branch(b).target();
    int source  = T.branch(b).source();
    Pr += prior_branch(A, P.branch_HMMs[b], target, source);
  }
  
  for(int i=T.n_leaves();i<T.n_nodes();i++)
    Pr -= 2.0*P.IModel().lengthp( A.seqlength(i) );
  return Pr;
}

double prior_HMM_notree(const alignment& A,const Parameters& P) {
  const Tree& T = P.T;

  int node = P.T.n_leafbranches();
  double Pr = P.IModel().lengthp(A.seqlength(node));

  for(int b=0;b<T.n_leafbranches();b++)
    Pr += prior_branch(A, P.branch_HMMs[b], node, b);

  if (T.n_leafbranches() > 1)
    Pr -= P.IModel().lengthp( A.seqlength(node) )*T.n_leaves();
  return Pr;
}

double Pr_tgaps_tletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM(A,P);
  Pr += substitution::Pr(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_tgaps_sletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM(A,P);
  Pr += substitution::Pr_star_estimate(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_tletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM(A,P);
  Pr += substitution::Pr(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_sletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM(A,P);
  Pr += substitution::Pr_star_estimate(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}


