#include <cmath>
#include "imodel.H"
#include "logsum.H"
#include "rng.H"
#include <gsl/gsl_randist.h>
#include "myexception.H"
#include "likelihood.H"

using std::vector;

// 0->0 1->G 2->3
int recode(int i,int G) {
  assert(G==1 or G==2);
  if (i==1)
    i=G;
  if (i==2)
    i=3;
  return i;
}

// FIXME - if we ever actually sample with a star gap model,
//         then we need to fix this.
double IndelModel::length_plus_p(int l, int G) const {
  return lengthp(l,G);
}

// f_M(s) = [ ME  + s(MGxGE - MExGG) ] / [ 1 - s(GG + MM) + s^2(MMxGG - MGxGM) ]

double IndelModel::lengthp(int l,int G) const {

  //--------------- Remove the 'G2' State ----------------------//
  Matrix Q2(2,3);

  double OMQ22 = logdiff(0,Q(2,2));
  for(int i=0;i<Q2.size1();i++) {
    int i1 = recode(i,G);
    for(int j=0;j<Q2.size2();j++) {
      int j1 = recode(j,G);
      Q2(i,j) = logsum(Q(i1,j1), Q(i1,2)+Q(2,j1)-OMQ22);
      Q2(i,j) = exp( Q2(i,j) );
    }
  }

  double MM = Q2(0,0);
  double MG = Q2(0,1);
  double ME = Q2(0,2);

  double GM = Q2(1,0);
  double GG = Q2(1,1);
  double GE = Q2(1,2);

  //----- Calculate roots of q(s); we assume its quadratic -----//
  double C = 1;
  double B = -(GG + MM);
  double A = MM*GG - MG*GM;

  double determinant = B*B-4.0*A*C;
  double r1 = (-B - sqrt(determinant))/(2*A);
  double r2 = (-B + sqrt(determinant))/(2*A);

  //------------ Calculate the coefficients f_M[l] ------------//
  double P;
  if (l==0)
    P = ME;
  else {
    // Calculate q[l] and q[l-i] (I've proved that all q[i]>=0)
    double q_l   = 1.0/ (A*(r2-r1)) * (pow(r1,-l-1) - pow(r2,-l-1));
    double q_lm1 = 1.0/ (A*(r2-r1)) * (pow(r1,-l)   - pow(r2,-l));

    // Calculate f_M[l] from the q[i] (*IS* this always positive?)
    P = ME*q_l + (MG*GE - ME*GG)*q_lm1;
  }
  return log(P);
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
    double pdel =  lambda_O-logdiff(0,lambda_O);
    double rate =  log(-logdiff(0,pdel));

    rate        += gaussian(0,sigma);
    pdel        =  logdiff(0,-exp(rate));
    lambda_O    =  pdel - logsum(0,pdel);
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


double IndelModel1::prior(double D) const {
  double P=0;

  // Calculate prior on lambda_O
  double lambda_O = parameters_[0];
  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel)) - log(D);

  P += log( shift_laplace_pdf(rate,-5, 0.5) );

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
    double pdel =  lambda_O-logdiff(0,lambda_O);
    double rate =  log(-logdiff(0,pdel));

    rate        += gaussian(0,sigma);
    pdel        =  logdiff(0,-exp(rate));
    lambda_O    =  pdel - logsum(0,pdel);
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
  double P = 0;

  // Calculate prior on lambda_O
  double lambda_O = parameters_[0];
  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel)) - log(D);

  P += log( shift_laplace_pdf(rate,-5, 0.5) );

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameters_[1];
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

  if (not fixed[0]) {
    double pdel =  lambda_O-logdiff(0,lambda_O);
    double rate =  log(-logdiff(0,pdel));

    rate        += gaussian(0,sigma);
    pdel        =  logdiff(0,-exp(rate));
    lambda_O    =  pdel - logsum(0,pdel);
  }
  
  recalc();
}

double SingleIndelModel::prior(double D) const {
  // Calculate prior on lambda_O
  double lambda_O = parameters_[0];
  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel)) - log(D);

  return log( shift_laplace_pdf(rate,-5, 0.5) );
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

