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
  SModel_->fiddle();recalc();
  IModel_->fiddle();
  
  const double sigma = 0.05;
  branch_mean += gaussian(0,sigma);
  if (branch_mean < 0) branch_mean = -branch_mean;
}

void Parameters::setlength(int b,double l) {
  assert(l >= 0);
  assert(b >= 0 and b < T.branches());
  T.branch(b).length() = l;
  for(int r=0;r<SModel().nrates();r++)
    transition_P_[r][b] = SModel_->transition_p(l,r);
}


void Parameters::recalc() {
  for(int b=0;b<T.branches();b++) 
    setlength(b,T.branch(b).length());
}


Parameters& Parameters::operator=(const Parameters& P) {
  transition_P_ = P.transition_P_;

  delete SModel_;
  SModel_ = P.SModel_->clone();

  delete IModel_;
  IModel_ = P.IModel_->clone();

  T = P.T;

  branch_mean = P.branch_mean;

  features = P.features;
  constants = P.constants;

  return (*this);
}

Parameters::Parameters(const Parameters& P):
  transition_P_(P.transition_P_),
  IModel_(P.IModel_->clone()),SModel_(P.SModel_->clone()),
  constants(P.constants),features(P.features),T(P.T),branch_mean(P.branch_mean)
{ }


Parameters::Parameters(const substitution::MultiRateModel& SM,const IndelModel& IM,const SequenceTree& t)
  :transition_P_(vector< vector <Matrix> >(SM.nrates(),
					  vector<Matrix>(t.branches(),
							 Matrix(SM.Alphabet().size(),
								SM.Alphabet().size()
								)
							 ) 
					  ) 
		 ),
   IModel_(IM.clone()),SModel_(SM.clone()),
   features(0),T(t)
{
  branch_mean = 0.1;
  recalc();
  constants.push_back(-1);
  
}
