#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "alignment.H"
#include "parameters.H"
#include "substitution.H"

double prior_no_tree(const alignment&,const Parameters&);
double prior_internal(const alignment&,const Parameters&);
double prior_HMM(const alignment&,const Parameters&);
double prior_HMM_Given(const alignment&,const Parameters&);
double prior_branch_HMM(const alignment&,const IndelModel&,int parent,int child);

inline double log_double_factorial(int n) {
  double x = 0;
  for(int i=3;i<=n;i+=2)
    x += log(i);
  return x;
}

inline double prior(const SequenceTree& T,double branch_mean) {
  double p = 0;

  /* ----- 1/(number of topologies) -----*/
  if (T.leaves()>3)
    p = -log_double_factorial(2*T.leaves()-5);

  /* ---- PROD_i exp(-lambda * T[i]) ---- */
  for(int i=0;i<T.branches();i++) 
    p -= branch_mean*T.branch(i).length();
  return p;
}

inline double prior(const Parameters& Theta) {
  double p = 0;

  p += prior(Theta.T,Theta.branch_mean);
  p += Theta.SModel().prior();

  return p;
}

/*  Now defunct - I don't want to maintain possibilities.C and gaps.C
double prior(const alignment&,const Parameters&);
// ln(P(A,Data|T)) = ln(P(A|T)) + ln(P(Data|A,T))
inline double probability_simple_tree(const alignment& A,const Parameters& Theta) {
  double p = 0;
  p += prior(A,Theta);  
  p += substitution(A,Theta); // also deals w/ frequencies
  p += prior(Theta);

  return p;
}
*/

// ln(P(A,Data|T)) = ln(P(A)) + ln(P(Data|A,T))
inline double probability_no_tree(const alignment& A,const Parameters& Theta) {
  double p = 0;
  p += prior_no_tree(A,Theta);  
  p += substitution(A,Theta); // also deals w/ frequencies
  p += prior(Theta);

  return p;
}

// ln(P(A,Data|T)) = ln(P(A)) + ln(P(Data|A,T))
inline double probability2(const alignment& A,const Parameters& Theta) {
  double p = 0;
  p += prior_internal(A,Theta);
  p += substitution(A,Theta); // also deals w/ frequencies
  p += prior(Theta);
  return p;
}

inline double probability3(const alignment& A,const Parameters& Theta) {
  double p = 0;
  p += prior_HMM_Given(A,Theta);
  p += substitution(A,Theta); // also deals w/ frequencies
  p += prior(Theta);
  return p;
}

#endif
