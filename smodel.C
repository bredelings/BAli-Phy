#include <fstream>
#include "smodel.H"
#include "exponential.H"
#include "rng.H"

namespace substitution {

// Q(i,j) = S(i,j)*pi[j]   for i!=j
// Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

// We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
// Then Q = S*D, and we can easily compute the exponential
// So, S(i,j) = Q(i,i)/pi[i]

void ReversibleModel::recalc() {

  // Set S(i,i) so that Q(i,i) = S(i,i)*pi[i]
  for(int i=0;i<S.size1();i++) {
    double sum=0;
    for(int j=0;j<S.size2();j++) {
      if (i==j) continue;
      sum += S(i,j)*pi[j];
    }
    S(i,i) = -sum/pi[i];
  }

  // Rescale so that expected mutation rate is 1
  double scale=0;
  for(int i=0;i<S.size1();i++) 
    scale += pi[i]*S(i,i)*pi[i];

  S /= -scale;

  // Move from 'S' to 'S+F'
  for(int i=0;i<S.size1();i++)
    for(int j=0;j<S.size2();j++)
      Q(i,j) = S(i,j)*pi[j];


  // Rescale so expected that mutation rate is 1
  scale = 0;
  for(int i=0;i<S.size1();i++) 
    scale += pi[i]*Q(i,i);

  std::cerr<<"scale = "<<scale<<endl;

  // Maybe assert that 
  //  A) the sum_j Q_ij = 0
  //  B) sum_i pi_i Q_ij = pi_j

}

Matrix ReversibleModel::transition_p(double t) const {
  BMatrix D(a.size(),a.size());
  for(int i=0;i<a.size();i++)
    D(i,i) = pi[i];

  return exp(S,D,t);
}


void HKY::fiddle() {
  const double sigma = 0.05;
  double k = kappa() + gaussian(0,sigma);
  if (k<0) k = -k;
  if (k==0) k = kappa();

  kappa(k);
}

double HKY::prior() const {
  return log(gsl_ran_lognormal_pdf(kappa(),0,0.1));
}

void HKY::recalc() {
  assert(a.size()==4);

  S(A,G) = kappa();
  S(A,C) = 1;
  S(A,T) = 1;

  S(G,A) = kappa();
  S(G,C) = 1;
  S(G,T) = 1;

  S(C,A) = 1;
  S(C,G) = 1;
  S(C,T) = kappa();

  S(T,A) = 1;
  S(T,G) = 1;
  S(T,C) = kappa();

  ReversibleModel::recalc();
}

void HKY::setup_alphabet() {
  A = a['A'];
  G = a['G'];
  C = a['C'];
  try {
    T = a['T'];
  }
  catch (bad_letter& e) {
    T = a['U'];
  }
}

void EQU::recalc() {
  for(int i=0;i<a.size();i++)
    for(int j=0;j<a.size();j++)
      S(i,j) = 1;

  ReversibleModel::recalc();
}

void Empirical::recalc() {
  ReversibleModel::recalc();
}

void Empirical::load_file(const char* filename) {
  std::ifstream ifile(filename);

  if (not ifile)
    throw myexception(string("Couldn't open file '")+filename+"'");

  for(int i=0;i<a.size();i++)
    for(int j=0;j<i;j++) {
      ifile>>S(i,j);
      S(j,i) = S(i,j);
    }

  for(int i=0;i<a.size();i++)
    ifile>>pi[i];
}


void NestedModel::parameters(const vector<double>& p) {
  vector<double> sub_p = sub_model->parameters();
  for(int i=0;i<sub_p.size();i++)
    sub_p[i] = p[i];

  SubModel().parameters(sub_p);
  
  Model::parameters(p);
}


double GammaRateModel::super_prior() const {
  return 1;
}

void GammaRateModel::super_fiddle() {
  double& alpha = parameters_[parameters_.size()-1];

  const double sigma = 0.1;
  alpha += gaussian(0,sigma);
  if (alpha < 0) alpha = -alpha;

  recalc();
}

double gamma_quantile(double x,double a, double b) {
  int max = 20;
  return x;
}


void GammaRateModel::recalc() {
  double alpha = parameters_[parameters_.size()-1];

  double mean=0;
  for(int i=0;i<nrates();i++) {
    rates_[i] = gamma_quantile(double(2*i+1)/(2.0*nrates()),alpha,1.0/alpha);
    mean += rates_[i];
    
  }
  mean /= nrates();

  for(int i=0;i<nrates();i++)
    rates_[i] /= mean;
}

GammaRateModel::GammaRateModel(const ReversibleModel& M,int n)
  :MultiRateWithBase(M,1,n)
{
  double& alpha = parameters_[parameters_.size()-1];
  alpha = 1.0;
  for(int i=0;i<nrates();i++)
    distribution_[i] = 1.0/nrates();

  recalc();
}




INV_Model::INV_Model(const MultiRateModel& M)
  :MultiRateModel(M,1,M.rates().size()+1)
{
  distribution_[ sub_model->distribution().size() ] = 0.0;
}    


void INV_Model::super_fiddle() {
  double &p = parameters_[parameters_.size()-1];
  const double sigma = 0.1;
  p += gaussian(0,sigma);
  if (p<0) p=-p;
  while(p>1)
    p--;

  recalc();
}

void INV_Model::recalc() {
  double p = parameters()[parameters().size()-1];

  distribution_[nrates()-1] = p;
  
  for(int i=0;i<sub_model->distribution().size();i++)
    distribution_[i] = sub_model->distribution()[i]*(1.0-p);
}

}
