#ifndef AIS_H
#define AIS_H

#include "mcmc.H"

class AIS_Sampler
{
  MCMC::Sampler S;

  void sample_from_beta(double beta,owned_ptr<Probability_Model>& P, int n, MCMC::Sampler& S0);

public:
  void go(owned_ptr<Probability_Model>& P, std::ostream&, std::vector<double> beta, int n);

  AIS_Sampler(const MCMC::Sampler& s): S(s) { }
};


#endif
