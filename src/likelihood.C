#include "likelihood.H"
#include "logsum.H"
#include "substitution.H"
#include "setup.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "probability.H"

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

  const double branch_mean_mean = 0.4;

  // prior on the mu, the mean branch length
  p += exp_exponential_pdf(log(P.branch_mean),branch_mean_mean);

  // prior on the topology and branch lengths
  p += prior(P.T, P.branch_mean);

  // prior on the substitution model
  p += P.SModel().prior();

  // prior on the insertion/deletion model
  p += P.IModel().prior(P.branch_mean);

  return p;
}

/// Probability of a pairwise alignment
double prior_branch(const alignment& A,const IndelModel& IModel,int target,int source) {
  vector<int> state = get_path(A,target,source);

  double P = log_0;
  for(int i=0;i<4;i++)
    P = logsum(P,IModel.pi[i] + IModel.Q(i,state[0]));

  for(int i=1;i<state.size();i++) 
    P += IModel.Q(state[i-1],state[i]);
  
  return P;
}

/// Probability of a pairwise alignment given the source length
double prior_branch_given(const alignment& A,const IndelModel& IModel,int target,int source) {
  double Pr = prior_branch(A,IModel,target,source);

  Pr -= IModel.lengthp(A.seqlength(source));

  return Pr;
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
    double p = prior_branch(A, P.IModel(),target,source);
    Pr += p;
  }
  
  return Pr;
}

//FIXME - this will have to change if we ever have sequences at internal nodes
//        that have other than 3 neighbors
double prior_HMM(const alignment& A,const Parameters& P) {
  const Tree& T = P.T;

  // collected the prior on independant pairwise alignment
  double Pr = prior_HMM_nogiven(A,P);

  // account for the condition that alignments agree on shared node lengths
  for(int i=T.n_leaves();i<T.n_nodes();i++)
    Pr -= P.IModel().lengthp(A.seqlength(i))*2.0;

  return Pr;
}

double prior_HMM_notree_nogiven(const alignment& A,const Parameters& P) {
  const Tree& T =P.T;

  double Pr = 0;
  for(int b=0;b<T.n_leafbranches();b++) {
    int target = b;
    int source  = T.n_leaves();
    // If only two sequences, then no central node
    if (source == 2) source = 1;
    double p = prior_branch(A, P.IModel(),target,source);
    Pr += p;
  }
  
  return Pr;
}


double prior_HMM_notree(const alignment& A,const Parameters& P) {
  const Tree& T =P.T;

  double Pr = prior_HMM_notree_nogiven(A,P);

  if (T.n_leaves() > 2)
    Pr -= T.n_leaves() * P.IModel().lengthp(A.seqlength(T.n_leaves()));

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
  Pr += prior_HMM_notree(A,P);
  Pr += substitution::Pr(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_sletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM_notree(A,P);
  Pr += substitution::Pr_star_estimate(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}


