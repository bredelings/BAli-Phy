#include "parameters.H"
#include "rng.H"


void Parameters::fiddle() {
  SModel_->fiddle();recalc();
  
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

  IModel = P.IModel;

  T = P.T;

  branch_mean = P.branch_mean;

  features = P.features;
  constants = P.constants;

  return (*this);
}

Parameters::Parameters(const Parameters& P):
  transition_P_(P.transition_P_),SModel_(P.SModel_->clone()),
  constants(P.constants),features(P.features),IModel(P.IModel),T(P.T),branch_mean(P.branch_mean)
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
   SModel_(SM.clone()),IModel(IM),
   features(0),
   T(t)
{
  branch_mean = 0.1;
  recalc();
  constants.push_back(-1);
  
}
