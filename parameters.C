#include "parameters.H"
#include "rng.H"
#include "substitution.H"
#include "likelihood.H"

double Parameters::likelihood(const alignment& A,const Parameters& P) const {
  if (SModel_->full_tree)
    return substitution::Pr(A,P);
  else
    return substitution::Pr_star(A,P);
}

double Parameters::prior(const alignment& A,const Parameters& P) const {
  if (IModel_->full_tree)
    return prior3(A,P);
  else
    return prior_HMM_notree(A,P);
}

bool Parameters::accept_MH(const alignment& A1,const Parameters& P1,
		 const alignment& A2,const Parameters& P2) const {
  double p1 = probability3(A1,P1);
  double p2 = probability3(A2,P2);

#ifndef NDEBUG
  std::cerr<<" MH ["<<p2-p1<<"] : ";
#endif

  if (myrandomf() < exp(p2-p1)) 
    return true;
  else
    return false;
}


void Parameters::fiddle() {
  SModel_->fiddle(s_fixed);
  recalc();
  
  const double sigma = 0.05;
  branch_mean += gaussian(0,sigma);
  if (branch_mean < 0) branch_mean = -branch_mean;
}

void Parameters::recalc() {
  MatCache::recalc(T,*SModel_);
}

Parameters::Parameters(const substitution::MultiRateModel& SM,const IndelModel& IM,const SequenceTree& t)
  :MatCache(t,SM),
   IModel_(IM.clone()),
   SModel_(SM.clone()),
   i_fixed(false,IModel_->parameters().size()),
   s_fixed(false,SModel_->parameters().size()),
   features(0),
   T(t)
{
  branch_mean = 0.1;
  constants.push_back(-1);
}
