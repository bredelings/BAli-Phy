#include "parameters.H"
#include "rng.H"
#include "substitution.H"
#include "likelihood.H"
#include "util.H"
#include "proposals.H"

using std::cerr;
using std::endl;

efloat_t Parameters::basic_likelihood(const alignment& A,const Parameters& P) const {
  if (SModel_->full_tree)
    return substitution::Pr(A,P);
  else
    return substitution::Pr_star(A,P);
}

efloat_t Parameters::basic_prior(const alignment& A,const Parameters& P) const 
{
  if (IModel_)
    return prior_HMM(A,P) * ::prior(P);
  else
    return ::prior(P);
}

bool Parameters::accept_MH(const alignment& A1,const Parameters& P1,
			   const alignment& A2,const Parameters& P2,
			   double rho) const 
{
  efloat_t p1 = probability(A1,P1);
  efloat_t p2 = probability(A2,P2);

  double ratio = rho*double(p2/p1);

  if (ratio >= 1 or myrandomf() < ratio) 
    return true;
  else
    return false;
}

bool Parameters::accept_MH(const alignment& A,const Parameters& P1,const Parameters& P2,
			   double rho) const {
  efloat_t p1 = likelihood(A,P1) * ::prior(P1);
  efloat_t p2 = likelihood(A,P2) * ::prior(P2);

  double ratio = rho*(p2/p1);

  if (ratio >= 1 or myrandomf() < ratio) 
    return true;
  else
    return false;
}

const IndelModel& Parameters::IModel() const
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

IndelModel& Parameters::IModel()
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

void Parameters::recalc_imodel() 
{
  if (IModel_) {
    for(int b=0;b<branch_HMMs.size();b++) 
    {
      // use the length, unless we are unaligned
      double t = T.branch(b).length();
      if (branch_HMM_type[b] == 1)
	t = -1;

      // compute and cache the branch HMM
      branch_HMMs[b] = IModel_->get_branch_HMM(t);
    }
  }
}

void Parameters::recalc_smodel() 
{
  // set the rate to one
  SModel_->set_rate(1);
  read();

  //invalidate cached conditional likelihoods in case the model has changed
  LC.invalidate_all();

  //invalidate the cached transition probabilities in case the model has changed
  MatCache::recalc(T,*SModel_);
}

void Parameters::recalc(const vector<int>& indices)
{
  bool s_changed = false;
  bool i_changed = false;

  for(int i=0;i<indices.size();i++) {
    int m = model_of_index[indices[i]];
    if (m == 0)
      s_changed=true;
    else if (m == 1)
      i_changed=true;
  }

  assert(not i_changed or has_IModel());

  if (s_changed)
    recalc_smodel();

  if (i_changed)
    recalc_imodel();
}

Model& Parameters::SubModels(int i)
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i==0)
    return SModel();
  else if (i==1)
    return IModel();
  else
    std::abort();
}

const Model& Parameters::SubModels(int i) const
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i==0)
    return SModel();
  else if (i==1)
    return IModel();
  else
    std::abort();
}

int Parameters::n_submodels() const
{
  if (IModel_) 
    return 2;
  else
    return 1;
}

string Parameters::super_parameter_name(int i) const
{
  if (i == 0)
    return "mu";
  else
    return ::parameter_name("f",i,1);
}

void Parameters::setlength(int b,double l) {
  MatCache::setlength(b,l,T,*SModel_); 
  if (IModel_) {
    double t = T.branch(b).length();
    if (branch_HMM_type[b] == 1)
      t = -1;
    branch_HMMs[b] = IModel_->get_branch_HMM(t);
  }
  LC.invalidate_branch(T,b);
}

double Parameters::branch_mean() const 
{
  return parameter(0);
}

void Parameters::branch_mean(double mu)
{
  parameter(0,mu);
}

Parameters::Parameters(const substitution::MultiModel& SM,const IndelModel& IM,const SequenceTree& t)
  :MatCache(t,SM),
   IModel_(IM),
   SModel_(SM),
   branch_HMMs(t.n_branches()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0),
   features(0),
   T(t),
   TC(star_tree(t.get_sequences())),
   LC(T,SModel())
{
  constants.push_back(-1);

  set_super_parameters(1);
  read();
  parameters_[0] = 0.1;

  for(int b=0;b<TC.n_branches();b++)
    TC.branch(b).set_length(-1);
}

Parameters::Parameters(const substitution::MultiModel& SM,const SequenceTree& t)
  :MatCache(t,SM),
   SModel_(SM),
   beta(2, 1.0),
   features(0),
   T(t),
   TC(star_tree(t.get_sequences())),
   LC(T,SModel())
{
  constants.push_back(-1);

  set_super_parameters(1);
  read();
  parameters_[0] = 0.1;

  for(int b=0;b<TC.n_branches();b++)
    TC.branch(b).set_length(-1);
}
