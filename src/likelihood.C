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


double log_double_factorial(int n) {
  double x = 0;
  for(int i=3;i<=n;i+=2)
    x += log(i);
  return x;
}

double log_num_branches(int n) {
  return log(2*n-3);
}

double log_num_topologies(int n) {
  return log_double_factorial(2*n-5);
}

double log_num_topologies_in_partition(int n1,int n2) {
  double total = log_num_topologies(n1) + log_num_topologies(n2);
  total += log_num_branches(n1) + log_num_branches(n2);
  return total;
}

/// FIXME if X ~ Exp(mu), then g(y) = exp(y-exp(y)/mu)/mu
///                         ln g(y) = log(mu) + (y-exp(y))/mu 


/// Tree prior: branch lengths & topology
double prior(const SequenceTree& T,double branch_mean) {
  double p = 0;

  /* ----- 1/(number of topologies) -----*/
  if (T.leaves()>3)
    p = -log_num_topologies(T.leaves());

  /* ---- PROD_i exp(- T[i] / mu )/ mu ---- */
  for(int i=0;i<T.branches();i++) 
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
  p += P.IModel().prior(); // prior(branch_mean)?

  return p;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch(const alignment& A,const indel::PairHMM& Q,int parent,int child) {
  vector<int> state = get_path(A,parent,child);

  efloat_t P = Q.start(state[0]);
  for(int i=1;i<state.size();i++) 
    P *= Q(state[i-1],state[i]);
  
  return log(P);
}

double prior_HMM_nogiven(const alignment& A,const Parameters& P) {
  const tree& T = P.T;

  double Pr = 0;

#ifndef NDEBUG
  check_internal_nodes_connected(A,P.T);
#endif
  
  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    double p = prior_branch(A, P.branch_HMMs[b], parent,child);
    Pr += p;
  }
  
  return Pr;
}

double prior_HMM(const alignment& A,const Parameters& P) {
  const tree& T = P.T;

#ifndef NDEBUG
  check_internal_nodes_connected(A,P.T);
#endif

  double Pr = 0;
  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    Pr += prior_branch(A, P.branch_HMMs[b], parent, child);
  }
  
  for(int i=T.n_leaves();i<T.n_nodes()-1;i++)
    Pr -= 2.0*P.IModel().lengthp( A.seqlength(i) );
  return Pr;
}

double prior_HMM_notree(const alignment& A,const Parameters& P) {
  const tree& T = P.T;

  int node = P.T.leafbranches();
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


