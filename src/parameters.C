#include "parameters.H"
#include "rng.H"
#include "substitution.H"
#include "likelihood.H"
#include "util.H"

efloat_t Parameters::basic_likelihood(const alignment& A,const Parameters& P) const {
  if (SModel_->full_tree)
    return substitution::Pr(A,P);
  else
    return substitution::Pr_star(A,P);
}

efloat_t Parameters::basic_prior(const alignment& A,const Parameters& P) const {
  if (IModel_->full_tree)
    return prior3(A,P);
  else
    //FIXME
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

void Parameters::recalc_imodel() {
  // recalculate the cached branch HMMs
  for(int b=0;b<branch_HMMs.size();b++)
    branch_HMMs[b] = IModel_->get_branch_HMM(T.branch(b).length());
}

void Parameters::recalc_smodel() {
  // set the rate to one
  SModel_->set_rate(1);

  //invalidate cached conditional likelihoods in case the model has changed
  LC.invalidate_all();

  //invalidate the cached transition probabilities in case the model has changed
  MatCache::recalc(T,*SModel_);
}

void Parameters::recalc() {
  recalc_smodel();
  recalc_imodel();
}


double Parameters::fiddle_smodel(int i) {
  double rho = 1;

  if (SModel_->parameters().size()) {
    // Fiddle substitution parameters and recalculate rate matrices
    rho = SModel_->fiddle(i);
    SModel_->set_rate(1);

    // Recalculate the branch transition matrices
    recalc_smodel();
  }

  double sigma = loadvalue(keys,"mu_sigma",0.20);
  double ratio = exp(gaussian(0,sigma)); 
  branch_mean *= ratio;

  rho *= ratio;

  return rho;
}

double Parameters::fiddle_imodel(int i) 
{
  double rho = IModel_->fiddle(i);
  recalc_imodel();
  return rho;
}

void Parameters::setlength(int b,double l) {
  MatCache::setlength(b,l,T,*SModel_); 
  branch_HMMs[b] = IModel_->get_branch_HMM(T.branch(b).length());
  LC.invalidate_branch(T,b);
}

Parameters::Parameters(const substitution::MultiModel& SM,const IndelModel& IM,const SequenceTree& t)
  :MatCache(t,SM),
   IModel_(IM),
   SModel_(SM),
   branch_HMMs(t.n_branches()),
   beta(2, 1.0),
   features(0),
   T(t),
   LC(T,SModel())
{
  branch_mean = 0.1;
  constants.push_back(-1);
}
