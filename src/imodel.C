#include <cmath>
#include "imodel.H"
#include "logsum.H"
#include "rng.H"
#include <gsl/gsl_randist.h>
#include "myexception.H"
#include "probability.H"
#include "util.H"
#include "2way.H"
#include "logsum.H"

using std::vector;
using namespace A2;

namespace indel {
  PairHMM::PairHMM(): Matrix(5,5) {}

  double PairHMM::start(int s) const {
    double total = log_0;
    for(int i=0;i<n_states();i++)
      total = logsum(total,(*this)(n_states(),i) + (*this)(i,s));
    return total;
  }

  vector<double> PairHMM::start_pi() const {
    vector<double> pi(n_states()-1);
    for(int i=0;i<n_states();i++)
      pi[i] = start_pi(i);
    return pi;
  }
};

string i_parameter_name(int i,int n) {
  if (i>=n)
    throw myexception()<<"substitution model: refered to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return string("pI") + convertToString(i);
}

void remove_one_state(Matrix& Q,int S) {
  assert(Q.size1() == Q.size2());

  double temp = logdiff(0,Q(S,S));

  // compute transitions from i!=S -> j  [ depends on Q(S->j) ]
  for(int i=0;i<Q.size1();i++) {
    if (i != S) {
      // compute transitions from i!=S -> j!=S  [ depends on Q(i->S) and Q(S->j) ]
      for(int j=0;j<Q.size2();j++) 
	if (j != S)
	  Q(i,j) = logsum(Q(i,j), Q(i,S) + Q(S,j) - temp);
    }
    Q(i,S) = log_0;
  }

  // compute transitions from S -> j  
  for(int j=0;j<Q.size2();j++) 
    Q(S,j) -= temp;

}

// f_M(s) = [ ME  + s(MGxGE - MExGG) ] / [ 1 - s(GG + MM) + s^2(MMxGG - MGxGM) ]

double SimpleIndelModel::lengthp(int l) const {
  using namespace states;

  //--------------- Remove the 'G2' State ----------------------//
  double MM = Q1(M,M);
  double MG = Q1(M,G1);
  double ME = Q1(M,E);

  double GM = Q1(G1,M);
  double GG = Q1(G1,G1);
  double GE = Q1(G1,E);

  //----- Calculate roots of q(s); we assume its quadratic -----//
  double C = 1;
  double B = -(GG + MM);
  double A = MM*GG - MG*GM;

  double sqr_det = sqrt(B*B-4.0*A*C);
  double r1 = (-B - sqr_det)/(2*A);
  double r2 = (-B + sqr_det)/(2*A);

  //------------ Calculate the coefficients f_M[l] ------------//
  double P;
  if (l==0)
    P = ME;
  else {
    double P1 = pow(r1,-l-1);
    double P2 = pow(r2,-l-1);

    // Calculate q[l] and q[l-i] (I've proved that all q[i]>=0)
    double q_l   = 1.0/ (A*(r2-r1)) * (P1 - P2);
    double q_lm1 = 1.0/ (A*(r2-r1)) * (P1*r1 - P2*r2);

    // Calculate f_M[l] from the q[i] (*IS* this always positive?)
    P = ME*q_l + (MG*GE - ME*GG)*q_lm1;
  }
  return log(P);
}

IndelModel::IndelModel(int s)
  : full_tree(true)
{ 
  set_n_parameters(s);
}


IndelModel::IndelModel()
  : full_tree(true)
{ }


IndelModel::~IndelModel() {}


void SimpleIndelModel::fiddle() {

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

indel::PairHMM SimpleIndelModel::get_branch_HMM(double) const {
  using namespace states;

  double delta   = exp(parameters_[0]);
  double e       = exp(parameters_[1]);
  double t     = exp(parameters_[2]);

  if (1 - 2*delta <0)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e >= 1)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta > 0 and delta <= 1);
  assert(e > 0 and e <= 1);
  
  indel::PairHMM Q;

  Q(S,S ) = log_0;
  Q(S,M ) = log(1 - 2*delta);
  Q(S,G1) = log(delta);
  Q(S,G2) = log(delta);
  Q(S,E ) = log_0;

  Q(M,S)   = log(1-t);
  Q(M,M)   = log_0;
  Q(M,G1)  = log_0;
  Q(M,G2)  = log_0;
  Q(M,E)   = log(t);

  Q(G1,S)  = log(1-e) + log(1-t);
  Q(G1,M)  = log_0;
  Q(G1,G1) = log(e) + log(1-t);
  Q(G1,G2) = log_0;
  Q(G1,E)  = log(t);

  Q(G2,S)  = log(1-e) + log(1-t);
  Q(G2,M)  = log_0;
  Q(G2,G1) = log_0;
  Q(G2,G2) = log(e) + log(1-t);
  Q(G2,E)  = log(t);

  Q(E,S)   = log_0;
  Q(E,M)   = log_0;
  Q(E,G1)  = log_0;
  Q(E,G2)  = log_0;
  Q(E,E)   = 0;

  remove_one_state(Q,states::S);

  Q(S,S ) = log_0;
  Q(S,M ) = 0;
  Q(S,G1) = log_0;
  Q(S,G2) = log_0;
  Q(S,E ) = log_0;

  return Q;
}


void SimpleIndelModel::recalc() {

  /* Chain with transitions to End state */
  Q1 = Q2 = get_branch_HMM(1);

  remove_one_state(Q1,states::G2);
  remove_one_state(Q2,states::G1);

  for(int i=0;i<Q1.size1();i++) {
    for(int j=0;j<Q1.size2();j++) {
      Q1(i,j) = exp( Q1(i,j) );
      Q2(i,j) = exp( Q2(i,j) );
    }
  }
}

double SimpleIndelModel::prior() const {
  double D = 0.5;
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

string SimpleIndelModel::name() const {return "simple indels [HMM]";}

string SimpleIndelModel::parameter_name(int i) const {
  if (i==0)
    return "SIMPLE::delta";
  else if (i==1)
    return "SIMPLE::epsilon";
  else if (i==2)
    return "SIMPLE::tau";
  else
    return i_parameter_name(i,3);
}

SimpleIndelModel::SimpleIndelModel()
  :IndelModel(3)
{
  parameters_[0] = -5;
  parameters_[1] = -0.5;
  parameters_[2] = log(.001);

  recalc();
}

void NewIndelModel::recalc() {
}

void NewIndelModel::fiddle() {
  double& rate = parameters_[0];
  double& lambda_E = parameters_[1];

  const double sigma = 0.35;

  if (not fixed[0]) {
    rate        += gaussian(0,sigma);
    if (rate > 0) 
      rate = -rate;
  }
  
  if (not fixed[1]) {
    double E_length = lambda_E - logdiff(0,lambda_E);
    E_length += gaussian(0,sigma);
    lambda_E = E_length - logsum(0,E_length);
  }
  
  recalc();
}

double NewIndelModel::prior() const {
  double P = 0;

  // Calculate prior on lambda_O
  double rate = parameters_[0];

  P += log( shift_laplace_pdf(rate,-5, 0.5) );

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameters_[1];
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = 5.0;

  P += exp_exponential_log_pdf(E_length,E_length_mean);

  return P;
}

indel::PairHMM NewIndelModel::get_branch_HMM(double t) const {
  using namespace states;

  double rate    = exp(parameters_[0]);
  double e = exp(parameters_[1]);

  // (1-e) * delta / (1-delta) = P(indel)
  double P_indel = 1.0 - exp(-rate*t);
  double A = P_indel/(1.0-e);
  double delta = A/(1+A);

  if (1 - 2*delta <0)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e >= 1)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta > 0 and delta <= 1);
  assert(e > 0 and e <= 1);
  
  indel::PairHMM Q;

  Q(S ,M ) = log_0;
  Q(S ,M ) = log(1 - 2*delta);
  Q(S ,G1) = log(delta);
  Q(S ,G2) = log(delta);
  Q(S ,E)  = 0;

  Q(M ,S ) = log(1-e);
  Q(M ,M ) = log(e);
  Q(M ,G1) = log_0;
  Q(M ,G2) = log_0;
  Q(M ,E)  = log_0;

  Q(G1,S ) = log(1-e);
  Q(G1,M ) = log_0;
  Q(G1,G1) = log(e);
  Q(G1,G2) = log_0;
  Q(G1,E ) = log_0;

  Q(G1,S ) = log(1-e);
  Q(G2,M ) = log_0;
  Q(G2,G1) = log_0;
  Q(G2,G2) = log(e);
  Q(G2,E ) = log_0;

  Q(E, S ) = log_0;
  Q(E ,M ) = log_0;
  Q(E ,G1) = log_0;
  Q(E ,G2) = log_0;
  Q(E ,E ) = 0;

  remove_one_state(Q,S);

  return Q;
}

string NewIndelModel::name() const {return "new indels [HMM]";}

string NewIndelModel::parameter_name(int i) const {
  if (i==0)
    return "NEW::lambda";
  else if (i==1)
    return "NEW::epsilon";
  else
    return i_parameter_name(i,2);
}

double NewIndelModel::lengthp(int l) const {
  return 1;
}

NewIndelModel::NewIndelModel()
  :IndelModel(2)
{
  parameters_[0] = -5;
  parameters_[1] = -0.5;

  recalc();
}
