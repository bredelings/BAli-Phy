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

namespace indel 
{
  PairHMM::PairHMM()
    : Matrix(5,5),
      start_pi_(5,0) 
  {
    for(int i=0;i<size1();i++)
      for(int j=0;j<size2();j++)
	(*this)(i,j) = 0;
  }

  double PairHMM::start(int s) const {
    double total = 0;
    for(int i=0;i<n_states();i++)
      total += start_pi(i)*(*this)(i,s);
    return total;
  }
}

string i_parameter_name(int i,int n) {
  if (i>=n)
    throw myexception()<<"substitution model: refered to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return string("pI") + convertToString(i);
}

void remove_one_state(Matrix& Q,int S) {
  assert(Q.size1() == Q.size2());

  double temp = 1.0 - Q(S,S);

  // compute transitions from i!=S -> j  [ depends on Q(S->j) ]
  for(int i=0;i<Q.size1();i++) {
    if (i != S) {
      // compute transitions from i!=S -> j!=S  [ depends on Q(i->S) and Q(S->j) ]
      for(int j=0;j<Q.size2();j++) 
	if (j != S)
	  Q(i,j) +=  Q(i,S) * Q(S,j) / temp;
    }
    Q(i,S) = 0;
  }

  // compute transitions from S -> j  
  for(int j=0;j<Q.size2();j++) 
    Q(S,j) /= temp;

}

/// Only continue from S1 if we don't go to S2
void exitize(Matrix& Q,double t,int S1,int S2)
{
  for(int i=0;i<5;i++)
    Q(S1,i) *= (1.0-t);
  Q(S1,S2) += t;
}

/// Only continue from S, it we don't stay in the fragment
void fragmentize(Matrix& Q,double e,int S)
{
  exitize(Q,e,S,S);
}


/// Modify a model on residues to a model on fragments of length L ~ Geometric(e)
void fragmentize(Matrix& Q,double e)
{
  using namespace states;

  fragmentize(Q,e,M);
  fragmentize(Q,e,G1);
  fragmentize(Q,e,G2);
}

// f_M(s) = [ ME  + s(MGxGE - MExGG) ] / [ 1 - s(GG + MM) + s^2(MMxGG - MGxGM) ]

efloat_t SimpleIndelModel::lengthp(int l) const 
{
  using namespace states;

  //--------------- Remove the 'G2' State ----------------------//
  double MM = QE(M,M);
  double MG = QE(M,G1);
  double ME = QE(M,E);

  double GM = QE(G1,M);
  double GG = QE(G1,G1);
  double GE = QE(G1,E);

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
  return P;
}

IndelModel::~IndelModel() {}


indel::PairHMM SimpleIndelModel::get_branch_HMM(double) const {
  using namespace states;

  double delta   = exp(parameter(0));
  double e       = exp(parameter(1));
  double t       = exp(parameter(2));

  if (delta > 0.5)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e >= 1.0)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta >= 0.0 and delta <= 1.0);
  assert(e > 0.0 and e <= 1.0);
  
  indel::PairHMM Q;

  Q(S ,S ) = 0;
  Q(S ,M ) = 1 - 2*delta;
  Q(S ,G1) = delta;
  Q(S ,G2) = delta;
  Q(S ,E ) = 0;

  Q(M ,S ) = 1;
  Q(G1,S ) = 1;
  Q(G2,S ) = 1;

  Q(E,E)   = 1;

  // For the states G1, G2 fragment lengths are Geometric(e)
  fragmentize(Q,e,G1);
  fragmentize(Q,e,G2);

  // For the states M, G1, G2 we might exit with probability t
  exitize(Q,t,M ,E);
  exitize(Q,t,G1,E);
  exitize(Q,t,G2,E);

  // When moving from another state, continue until we are not in S
  remove_one_state(Q,S);

  Q.start_pi(S)  = 0;
  Q.start_pi(M)  = 1;
  Q.start_pi(G1) = 0;
  Q.start_pi(G2) = 0;
  Q.start_pi(E)  = 0;

  return Q;
}


void SimpleIndelModel::recalc(const vector<int>&) 
{
  /* Chain with transitions to End state */
  Q1 = get_branch_HMM(1);

  remove_one_state(Q1,states::G2);

  QE = Q1;
}

efloat_t SimpleIndelModel::prior() const 
{
  double D = 0.5;
  efloat_t Pr = 1;

  // Calculate prior on lambda_O
  double lambda_O = parameter(0);
  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel)) - log(D);

  Pr *= laplace_pdf(rate,-5, 0.5);

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameter(1);
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = 5.0;

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  return Pr;
}

string SimpleIndelModel::name() const {return "simple indels [HMM]";}

SimpleIndelModel::SimpleIndelModel()
  :QE(Q1.size1(),Q1.size2())
{
  add_parameter("delta",  -5);
  add_parameter("epsilon",-0.5);
  add_parameter("tau",    log(0.001));

  recalc_all();
}

efloat_t NewIndelModel::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda_O
  double rate = parameter(0);

  Pr *= laplace_pdf(rate,parameter(3), parameter(4));

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameter(1);
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = parameter(5);

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  // Calculate prior on invariant fraction
  if (not fixed(2)) {
    double i = parameter(2);
    Pr *= beta_pdf(i,1,25);
  }

  return Pr;
}

indel::PairHMM NewIndelModel::get_branch_HMM(double t) const 
{
  using namespace states;

  if (not time_dependant)
    t = 1;

  double rate    = exp(parameter(0));
  double e = exp(parameter(1));
  double i = parameter(2);

  // (1-e) * delta / (1-delta) = P(indel)
  // But move the (1-e) into the RATE to make things work
  double mu = rate*t/(1.0-e);
  double P_indel = 1.0 - exp(-mu);
  double A = P_indel*(1.0-i);
  double delta = A/(1+A);

  if (t < -0.5)
    delta = 0.5;

  if (1 - 2*delta <0)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e >= 1)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta >= 0 and delta <= 1);
  assert(e > 0 and e <= 1);
  
  // transition probabilities default to *zero*
  indel::PairHMM Q;

  Q(S ,S ) = 0;
  Q(S ,M ) = 1 - 2*delta;
  Q(S ,G1) = delta;
  Q(S ,G2) = delta;
  Q(S ,E ) = 1 - delta;

  Q(M ,S ) = 1;
  Q(G1,S ) = 1;
  Q(G2,S ) = 1;

  Q(E ,E ) = 1;

  // unless this branch is disconnected...
  if (t < -0.5) 
    ;
  // turn the model into a fragment model
  else
    fragmentize(Q,e);

  remove_one_state(Q,S);

  Q.start_pi(S)  = 0;
  Q.start_pi(M)  = 1;
  Q.start_pi(G1) = 0;
  Q.start_pi(G2) = 0;
  Q.start_pi(E)  = 0;

  return Q;
}

string NewIndelModel::name() const 
{
  string s = "fragment-based indels ";
  
  if (time_dependant)
    s += "+ T ";
  s += "[HMM]";
  return s;
}

efloat_t NewIndelModel::lengthp(int l) const 
{
  double e = exp(parameter(1));
  if (l < 0)
    return 0;
  else if (l==0)
    return 1.0;
  else
    return (1.0-e);
}

NewIndelModel::NewIndelModel(bool b)
  :time_dependant(b)
{
  add_parameter("lambda",   -5);
  add_parameter("epsilon",  -0.5);
  add_parameter("invariant",0.1);
  add_parameter("lambda::prior_median", -5);
  add_parameter("lambda::prior_stddev", 1.5);
  add_parameter("epsilon::prior_length", 5);
}


efloat_t TKF1::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(parameter(0),parameter(2), parameter(3));

  // Calculate prior on mean sequence length
  Pr *= exponential_pdf(parameter(1), parameter(4));

  return Pr;
}


// lambda is the insertion rate.
// mu     is the deletion  rate.
indel::PairHMM get_TKF1_HMM(double t,double lambda, double mu)
{
  using namespace states;

  indel::PairHMM Q;

  double U = exp(-mu*t);
  double B = (1.0 - exp((lambda-mu)*t))/(mu - lambda*exp((lambda - mu)*t));

  Q(S ,S ) = 0;
  Q(S ,M ) = (1.0 - lambda*B) * (lambda/mu) * U;
  Q(S ,G1) = lambda * B;
  Q(S ,G2) = (1.0 - lambda*B) * (lambda/mu) * (1.0-U);
  Q(S ,E)  = (1.0 - lambda*B) * (1.0 - lambda/mu);

  Q(M ,S ) = 0;
  Q(M ,M ) = Q(S, M);
  Q(M ,G1) = Q(S, G1);
  Q(M ,G2) = Q(S, G2);
  Q(M ,E)  = Q(S, E);

  Q(G1,S ) = 0;
  Q(G1,M ) = lambda * B * U/(1.0-U);
  Q(G1,G1) = lambda * B;
  Q(G1,G2) = (1.0 - U - mu*B)/(1.0-U);
  Q(G1,E ) = (mu-lambda)*B/(1.0-U);

  Q(G2,S ) = 0;
  Q(G2,M ) = Q(S, M);
  Q(G2,G1) = Q(S, G1);
  Q(G2,G2) = Q(S, G2);
  Q(G2,E ) = Q(S, E);

  Q(E, S ) = 0;
  Q(E ,M ) = 0;
  Q(E ,G1) = 0;
  Q(E ,G2) = 0;
  Q(E ,E ) = 1;

  Q.start_pi(S)  = 0;
  Q.start_pi(M)  = 1;
  Q.start_pi(G1) = 0;
  Q.start_pi(G2) = 0;
  Q.start_pi(E)  = 0;

  return Q;
  
}

indel::PairHMM TKF1::get_branch_HMM(double t) const 
{
  if (not time_dependant)
    t = 1;

  double lambda = exp(parameter(0));
  double mean_length = parameter(1);
  double sigma = mean_length/(1.0 + mean_length); // E L = s/(1-s)
  double mu = lambda/sigma;                       // s = lambda/mu

  assert(lambda < mu);

  return get_TKF1_HMM(t,lambda,mu);
}

string TKF1::name() const 
{
  return "TKF1";
}

efloat_t TKF1::lengthp(int l) const 
{
  double mean_length = parameter(1);

  double sigma = mean_length/(1.0 + mean_length);

  return (1.0-sigma)*pow<efloat_t>(sigma,l);
}

TKF1::TKF1(bool b)
  :time_dependant(b)
{
  add_parameter("lambda",-5);
  add_parameter("mean_length",100);
  add_parameter("lambda::prior_median", -5);
  add_parameter("lambda::prior_stddev", 1.5);
  add_parameter("mean_length::prior_mean", 1.5);
}


efloat_t TKF2::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(parameter(0),parameter(3), parameter(4));

  // Calculate prior on epsilon
  double lambda_E = parameter(1);
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = parameter(5);

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  // Calculate prior on mean sequence length
  Pr *= exponential_pdf(parameter(2), parameter(6));

  return Pr;
}

indel::PairHMM TKF2::get_branch_HMM(double t) const 
{
  using namespace states;

  if (not time_dependant)
    t = 1;

  double lambda = exp(parameter(0));
  double e = exp(parameter(1));
  double mean_length = parameter(2);
  double sigma = mean_length/(1.0 + mean_length); // E L = s/(1-s)
  double mu = lambda/sigma;                       // s = lambda/mu

  assert(lambda < mu);

  indel::PairHMM Q = get_TKF1_HMM(t,lambda,mu);
  fragmentize(Q,e);
  return Q;
}

string TKF2::name() const 
{
  return "TKF2";
}

efloat_t TKF2::lengthp(int l) const 
{
  // FIXME -  this is wrong
  std::abort();
  double mean_length = parameter(1);

  double sigma = mean_length/(1.0 + mean_length);

  return (1.0-sigma)*pow<efloat_t>(sigma,l);
}

TKF2::TKF2(bool b)
  :time_dependant(b)
{
  add_parameter("lambda",-5);
  add_parameter("epsilon",-0.5);
  add_parameter("mean_length",100);
  add_parameter("lambda::prior_median", -5);
  add_parameter("lambda::prior_stddev", 1.5);
  add_parameter("epsilon::prior_length", 5);
  add_parameter("mean_length::prior_mean", 1.5);
}
