#include "parameters.H"
#include "rng.H"
#include "substitution.H"
#include "likelihood.H"

double Parameters::basic_likelihood(const alignment& A,const Parameters& P) const {
  if (SModel_->full_tree)
    return substitution::Pr(A,P);
  else
    return substitution::Pr_star(A,P);
}

double Parameters::basic_prior(const alignment& A,const Parameters& P) const {
  if (IModel_->full_tree)
    return prior3(A,P);
  else
    return prior_HMM_notree(A,P);
}

double Parameters::weight(const alignment& A,const Parameters& P) const {
  double Pr = basic_prior(A,P) + basic_likelihood(A,P);
  return Pr * (1.0-1.0/P.Temp);
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
  if (SModel_->parameters().size()) {
    // Fiddle substitution parameters and recalculate rate matrices
    SModel_->fiddle(s_fixed);

    // Recalculate the branch transition matrices
    recalc();
  }

  double x = log(branch_mean);
  const double sigma = 0.10;
  x += gaussian(0,sigma);
  branch_mean = exp(x);
}

void Parameters::recalc() {
  MatCache::recalc(T,*SModel_);
}

void Parameters::setlength(int b,double l) {
  MatCache::setlength(b,l,T,*SModel_); 
  LC.invalidate_branch(T,b,true);
}

Parameters::Parameters(const substitution::MultiModel& SM,const IndelModel& IM,const SequenceTree& t)
  :MatCache(t,SM),
   IModel_(IM.clone()),
   SModel_(SM.clone()),
   Temp(1.0),
   i_fixed(false,IModel_->parameters().size()),
   s_fixed(false,SModel_->parameters().size()),
   features(0),
   T(t),
   LC(T,SModel())
{
  branch_mean = 0.1;
  constants.push_back(-1);
}
