#include "likelihood.H"
#include "logsum.H"
#include "substitution.H"
#include "setup.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>


double prior3(const alignment& A,const Parameters& P) {
  return prior_HMM(A,P) + prior(P);
}

double likelihood3(const alignment& A,const Parameters& P) {
  return substitution::Pr(A,P); // also deals w/ frequencies
}

double probability3(const alignment& A,const Parameters& P) {
  return likelihood3(A,P) + prior3(A,P);
}


/// log density for y if y=ln (x+delta), and x ~ Exp(mu)

/// f(x) = exp(-x/mu)/mu   g(y) = exp(-(exp(y)-delta)/mu)/mu * exp(y)
double exp_exponential_log_pdf(double y, double mu, double delta) {
  double x = exp(y)-delta;
  return -log(mu) -x/mu + y;
}

double exp_exponential_pdf(double y, double mu, double delta) {
  return exp(exp_exponential_log_pdf(y,mu));
}

double shift_laplace_pdf(double x, double mu, double sigma) {
  double a = sigma/sqrt(2);
  return gsl_ran_laplace_pdf(x-mu,a);
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
  p += P.IModel().prior(P.branch_mean);

  return p;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch(const alignment& A,const IndelModel& IModel,int parent,int child) {
  vector<int> state = get_path(A,parent,child);

  double P = log_0;
  for(int i=0;i<4;i++)
    P = logsum(P,IModel.pi[i] + IModel.Q(i,state[0]));

  for(int i=1;i<state.size();i++) 
    P += IModel.Q(state[i-1],state[i]);
  
  return P;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch_Given(const alignment& A,const IndelModel& IModel,int parent,int child) {
  double Pr = prior_branch(A,IModel,parent,child);

  Pr -= IModel.lengthp(A.seqlength(parent));

  return Pr;
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
    double p = prior_branch(A, P.IModel(),parent,child);
    Pr += p;
  }
  
  return Pr;
}

double prior_HMM(const alignment& A,const Parameters& P) {
  const tree& T = P.T;

#ifndef NDEBUG
  check_internal_nodes_connected(A,P.T);
#endif

  int highest_node = T.get_nth(T.num_nodes()-2);
  highest_node = T.branch_up(highest_node).parent();
  double Pr = P.IModel().lengthp(A.seqlength(highest_node));

  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    Pr += prior_branch_Given(A, P.IModel(), parent, child);
  }
  
  return Pr;
}

double prior_branch_notree_nogiven(const alignment& A,const IndelModel& IModel,int child) {
  const vector<double>& pi = IModel.pi;
  const Matrix& Q = IModel.Q;

  vector<int> state(A.length()+1);
  for(int column=0;column<A.length();column++) {
    state[column] = 0;
    if (A.gap(column,child))
      state[column] = 2;
  }
  state[A.length()] = 3;

  double Pr = log_0;
  for(int i=0;i<4;i++)
    Pr = logsum(Pr,pi[i] + Q(i,state[0]) );

  for(int i=1;i<state.size();i++) 
    Pr += Q(state[i-1],state[i]);
  
  return Pr;
}

double prior_branch_notree(const alignment& A,const IndelModel& IModel,int child) {
  double Pr = prior_branch_notree_nogiven(A,IModel,child);
  Pr -= IModel.length_plus_p(A.length());
  return Pr;
}

double prior_HMM_notree(const alignment& A,const Parameters& P) {
  const tree& T =P.T;

  double Pr = P.IModel().lengthp(A.length());
  for(int b=0;b<T.branches();b++) 
    Pr += prior_branch_notree(A, P.IModel(), b);

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


