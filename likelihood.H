#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "alignment.H"
#include "parameters.H"
#include "substitution.H"

double prior_no_tree(const alignment&,const Parameters&);
double prior_internal(const alignment&,const Parameters&);
double prior_HMM(const alignment&,const Parameters&);

inline double prior(const Parameters& Theta) {
  double p = 0;
  for(int i=0;i<Theta.T.branches();i++) 
    p -= Theta.branch_mean*Theta.T.branch(i).length();
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
  p += prior_HMM(A,Theta);
  p += substitution(A,Theta); // also deals w/ frequencies
  p += prior(Theta);
  return p;
}

#endif
