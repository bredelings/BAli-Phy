#include "imodel.H"
#include "logsum.H"
#include "dp-engine.H"
#include "rng.H"
#include <gsl/gsl_randist.h>
#include "myexception.H"
#include "likelihood.H"

using std::vector;

void IndelModel::construct_length_plus_p() {
  const int n = 200;

  vector<double>& f_E = p_length_plus;

  vector<double> f_M(n);
  vector<double> f_G2(n);
  f_E.resize(n);



  f_M[0] = pi[0];
  f_G2[0] = pi[2];
  f_E[0] = pi[3]; 

  for(int i=1;i<n;i++) {
    f_M[i] = logsum(f_M[i-1]+Q(0,0),f_G2[i-1]+Q(2,0));
    f_G2[i] = logsum(f_M[i-1]+Q(0,2),f_G2[i-1]+Q(2,2));
    f_E[i] = logsum(f_M[i]+Q(0,3),f_G2[i]+Q(2,3));
  }
}

void IndelModel::construct_lengthp() {
  const int size1=50;
  const int size2=100;

  // Store emission characteristics
  vector<int> state_emit(4,0);
  state_emit[0] |= (1<<1)|(1<<0);
  state_emit[1] |= (1<<1);
  state_emit[2] |= (1<<0);
  state_emit[3] |= 0;

  // Store start probabilities
  vector<double> start_P = pi;
  start_P.erase(start_P.begin()+3);

  // Compute probabilities for pairs of lengths
  DPmatrixNoEmit Matrices(size1,size2,state_emit,start_P,Q,1.0);
  Matrices.forward_cell(0,0,0,0);
  Matrices.forward_square(0,0,size1,size2);

  // Compute probabilities for a single length
  vector<double> l1;
  //  vector<double> l2;
  
  for(int i=0;i<Matrices.size1();i++) {
    double total1 = log_0;
    //    double total2 = log_0;
    for(int j=0;j<Matrices.size2();j++) {
      for(int S=0;S<Matrices.nstates();S++) {
	total1 = logsum(total1,Matrices[S](i,j)+Q(S,3));
	//	total2 = logsum(total2,Matrices[S](j,i)+Q(S,3));
      }
    }
    l1.push_back(total1);
    //    l2.push_back(total2);
  }
  p_length = l1;

  //  for(int i=0;i<p_length.size();i++) {
  //    assert(l1[i] == l2[i]);
  //  }
}

IndelModel::IndelModel(int s)
  : P(4,4),parameters_(s),full_tree(true),pi(4),Q(4,4)
{ }


IndelModel::IndelModel()
  : P(4,4),full_tree(true),pi(4),Q(4,4)
{ }


void IndelModel1::fiddle(const std::valarray<bool>& fixed) { 

  double& lambda_O = parameters_[0];
  double& lambda_E = parameters_[1];

  const double sigma = 0.35;

  if (not fixed[0]) {
    lambda_O += gaussian(0,sigma);
    lambda_O = std::abs(lambda_O);
  }
  
  if (not fixed[1]) {
    double E_length = lambda_E - logdiff(0,lambda_E);
    E_length += gaussian(0,sigma);
    lambda_E = E_length - logsum(0,E_length);
  }
  
  recalc();
}

void IndelModel1::recalc() {
  double delta   = exp(parameters_[0]);
  double epsilon = exp(parameters_[1]);
  double tau     = 1.0e-3;

  assert(delta > 0.0);
  
  /* Chain w/o transitions to End state */
  P(0,0) = log(1.0-delta-delta*(1.0-delta) );
  P(0,1) = log(delta);
  P(0,2) = log(delta *(1.0-delta) );
  P(0,3) = log_0;

  P(1,0) = log(1.0 - epsilon) + log(1.0 - delta);
  P(1,1) = log(epsilon);
  P(1,2) = log(1.0 - epsilon) + log(delta);
  P(1,3) = log_0;

  P(2,0) = log(1.0 - epsilon);
  P(2,1) = log_0;
  P(2,2) = log(epsilon);
  P(2,3) = log_0;

  P(3,0) = log_0;
  P(3,1) = log_0;
  P(3,2) = log_0;
  P(3,3) = 0;

  /* Chain with transitions to End state */
  Q = P;
  for(int i=0;i<3;i++) {
    for(int j=0;j<3;j++) 
      Q(i,j) += log(1.0 - tau);
    Q(i,3) = log(tau);
  }

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = 0;
  pi[1] = log_0;
  pi[2] = log_0;
  pi[3] = log_0;  // must be log_0

  //  pi[0] = log(1.0-delta-delta*(1.0-delta) );
  //  pi[1] = log(delta);
  //  pi[2] = log(delta *(1.0-delta) );
  //  pi[3] = log_0;  // must be log_0

  IndelModel::recalc();
}

IndelModel::~IndelModel() {}


double delta_prior(double D,double delta) {
  double lambda = -log(1.0-delta)/D;

  // Calculate prior on lambda_O
  const double mean_delta = exp(-6.5);
  const double mean_D     = 0.4;
  const double mean_lambda = -log(1.0-mean_delta)/mean_D;

  // mean_lambda = exp(-5.5)/0.25 (approximation based on log(1+x) = x + ...)

  // This doesn't allow mu to be negative... just very small...
  return log( gsl_ran_gaussian_pdf(log(lambda)-log(mean_lambda),0.5) );
}

// P(no insertion) = 1-delta = exp(-lambda*D)
// lambda = insertion rate
double IndelModel1::prior(double D) const {

  double delta = exp(parameters_[0]);

  double P = delta_prior(D,delta);

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameters_[1];
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = 5.0;

  P += exp_exponential_log_pdf(E_length,E_length_mean);

  return P;
}

IndelModel1::IndelModel1(double lambda_O,double lambda_E)
  :IndelModel(2)
{
  parameters_[0] = lambda_O;
  parameters_[0] = lambda_E;

  recalc();
}

void UpweightedIndelModel::fiddle(const std::valarray<bool>& fixed) { 

  double& lambda_O = parameters_[0];
  double& lambda_E = parameters_[1];

  const double sigma = 0.35;

  if (not fixed[0]) {
    lambda_O += gaussian(0,sigma);
    lambda_O = std::abs(lambda_O);
  }
  
  if (not fixed[1]) {
    double E_length = lambda_E - logdiff(0,lambda_E);
    E_length += gaussian(0,sigma);
    lambda_E = E_length - logsum(0,E_length);
  }
  
  recalc();
}

void UpweightedIndelModel::recalc() {
  double delta   = exp(parameters_[0]);
  double epsilon = exp(parameters_[1]);
  double tau     = 1.0e-3;

  if (1.0 - 2.0*delta <0.0)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (epsilon >= 1.0)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<epsilon;
    
  assert(delta > 0 and delta <= 1);
  assert(epsilon > 0 and epsilon <= 1);
  
  /* Chain w/o transitions to End state */
  P(0,0) = log(1.0 - 2.0*delta);
  P(0,1) = log(delta);
  P(0,2) = log(delta);
  P(0,3) = log_0;

  P(1,0) = log(1.0 - epsilon) + log(1.0 - 2.0*delta);
  P(1,1) = log(epsilon + (1.0-epsilon)*delta);
  P(1,2) = log(1.0 - epsilon) + log(delta);
  P(1,3) = log_0;

  P(2,0) = log(1.0 - epsilon) + log(1.0 - 2.0*delta);
  P(2,1) = log(1.0 - epsilon) + log(2.0*delta/2.0);
  P(2,2) = log(epsilon + (1.0-epsilon)*delta);
  P(2,3) = log_0;

  P(3,0) = log_0;
  P(3,1) = log_0;
  P(3,2) = log_0;
  P(3,3) = 0;

  /* Chain with transitions to End state */
  Q = P;
  for(int i=0;i<3;i++) {
    for(int j=0;j<3;j++) 
      Q(i,j) += log(1.0 - tau);
    Q(i,3) = log(tau);
  }

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = 0;
  pi[1] = log_0;
  pi[2] = log_0;
  pi[3] = log_0;  // must be log_0

  //  pi[0] = log(1.0-2.0*delta);
  //  pi[1] = log(delta);
  //  pi[2] = log(delta);
  //  pi[3] = log_0;  // must be log_0

  IndelModel::recalc();
}

double UpweightedIndelModel::prior(double D) const {
  double lambda_O = parameters_[0];
  double lambda_E = parameters_[1];

  // Calculate prior on lambda_O
  double delta = exp(lambda_O);
  double P = delta_prior(D,delta);

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = 5.0;

  P += exp_exponential_log_pdf(E_length,E_length_mean);

  return P;
}

UpweightedIndelModel::UpweightedIndelModel(double lambda_O,double lambda_E)
  :IndelModel(2)
{
  parameters_[0] = lambda_O;
  parameters_[1] = lambda_E;

  recalc();
}

void SingleIndelModel::fiddle(const std::valarray<bool>& fixed) { 
  double& lambda_O = parameters_[0];

  const double sigma = 0.35;

  lambda_O += gaussian(0,sigma);
  if (lambda_O >= 0) lambda_O = -lambda_O;

  recalc();
}

double SingleIndelModel::prior(double D) const {
  double delta = exp(parameters_[0]);
  return delta_prior(D,delta);
}

void SingleIndelModel::recalc() {
  double delta = exp(parameters_[0]);
  double tau   = 1.0e-3;

  assert(delta > 0 and delta <= 1);

  /* Chain w/o transitions to End state */
  P(0,0) = log(1.0 - 2.0*delta);
  P(0,1) = log(delta);
  P(0,2) = log(delta);
  P(0,3) = log_0;

  P(1,0) = log(1.0 - 2.0*delta);
  P(1,1) = log(delta);
  P(1,2) = log(delta);
  P(1,3) = log_0;

  P(2,0) = log(1.0 - 2.0*delta);
  P(2,1) = log(delta);
  P(2,2) = log(delta);
  P(2,3) = log_0;

  P(3,0) = log_0;
  P(3,1) = log_0;
  P(3,2) = log_0;
  P(3,3) = 0;

  /* Chain with transitions to End state */
  Q = P;
  for(int i=0;i<3;i++) {
    for(int j=0;j<3;j++) 
      Q(i,j) += log(1.0 - tau);
    Q(i,3) = log(tau);
  }

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = 0;
  pi[1] = log_0;
  pi[2] = log_0;
  pi[3] = log_0;  // must be log_0

  IndelModel::recalc();
}

SingleIndelModel::SingleIndelModel(double LO)
  :IndelModel(1)
{
  parameters_[0] = LO;
  recalc();
}

