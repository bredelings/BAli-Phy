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
    double total = 0;
    for(int i=0;i<n_states();i++)
      total += (*this)(n_states(),i)*(*this)(i,s);
    return total;
  }

  vector<double> PairHMM::start_pi() const {
    vector<double> pi(n_states()-1);
    for(int i=0;i<pi.size();i++)
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

// f_M(s) = [ ME  + s(MGxGE - MExGG) ] / [ 1 - s(GG + MM) + s^2(MMxGG - MGxGM) ]

efloat_t SimpleIndelModel::lengthp(int l) const {
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

IndelModel::IndelModel(int s)
  : full_tree(true)
{ 
  set_n_parameters(s);
}


IndelModel::IndelModel()
  : full_tree(true)
{ }


IndelModel::~IndelModel() {}


double SimpleIndelModel::fiddle(int) {

  double& lambda_O = parameters_[0];
  double& lambda_E = parameters_[1];

  const double sigma = 0.35;

  if (not fixed(0)) {
    double pdel =  lambda_O-logdiff(0,lambda_O);
    double rate =  log(-logdiff(0,pdel));

    rate        += gaussian(0,sigma);
    pdel        =  logdiff(0,-exp(rate));
    lambda_O    =  pdel - logsum(0,pdel);
  }
  
  if (not fixed(1)) {
    double E_length = lambda_E - logdiff(0,lambda_E);
    E_length += gaussian(0,sigma);
    lambda_E = E_length - logsum(0,E_length);
  }
  
  recalc();
  return 1;
}

indel::PairHMM SimpleIndelModel::get_branch_HMM(double) const {
  using namespace states;

  double delta   = exp(parameters_[0]);
  double e       = exp(parameters_[1]);
  double t       = exp(parameters_[2]);

  if (delta > 0.5)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e >= 1.0)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta > 0.0 and delta <= 1.0);
  assert(e > 0.0 and e <= 1.0);
  
  indel::PairHMM Q;

  Q(S,S ) = 0;
  Q(S,M ) = 1 - 2*delta;
  Q(S,G1) = delta;
  Q(S,G2) = delta;
  Q(S,E ) = 0;

  Q(M,S)   = 1-t;
  Q(M,M)   = 0;
  Q(M,G1)  = 0;
  Q(M,G2)  = 0;
  Q(M,E)   = t;

  Q(G1,S)  = (1-e) * (1-t);
  Q(G1,M)  = 0;
  Q(G1,G1) = e * (1-t);;
  Q(G1,G2) = 0;
  Q(G1,E)  = t;

  Q(G2,S)  = (1-e) * (1-t);
  Q(G2,M)  = 0;
  Q(G2,G1) = 0;
  Q(G2,G2) = e * (1-t);
  Q(G2,E)  = t;

  Q(E,S)   = 0;
  Q(E,M)   = 0;
  Q(E,G1)  = 0;
  Q(E,G2)  = 0;
  Q(E,E)   = 1;

  remove_one_state(Q,states::S);

  Q(S,S ) = 0;
  Q(S,M ) = 1;
  Q(S,G1) = 0;
  Q(S,G2) = 0;
  Q(S,E ) = 0;

  return Q;
}


void SimpleIndelModel::recalc() {

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
  double lambda_O = parameters_[0];
  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel)) - log(D);

  Pr *= shift_laplace_pdf(rate,-5, 0.5);

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameters_[1];
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = 5.0;

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  return Pr;
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
  :IndelModel(3),QE(Q1.size1(),Q1.size2())
{
  parameters_[0] = -5;
  parameters_[1] = -0.5;
  parameters_[2] = log(.001);

  recalc();
}

void NewIndelModel::recalc() {
}

double NewIndelModel::fiddle(int) {
  double& rate = parameters_[0];
  double& lambda_E = parameters_[1];
  double& i = parameters_[2];

  const double sigma = 0.35;

  if (not fixed(0)) {
    rate        += gaussian(0,sigma);
    if (rate > 0) 
      rate = -rate;
  }
  
  if (not fixed(1)) {
    double E_length = lambda_E - logdiff(0,lambda_E);
    E_length += gaussian(0,sigma);
    lambda_E = E_length - logsum(0,E_length);
  }

  if (not fixed(2))
    i = wrap(i+gaussian(0,0.02),1.0);
  
  recalc();
  return 1;
}

efloat_t NewIndelModel::prior() const {
  efloat_t Pr = 1;

  // Calculate prior on lambda_O
  double rate = parameters_[0];

  Pr *= shift_laplace_pdf(rate,parameters_[3], parameters_[4]);

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = parameters_[1];
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = parameters_[5];

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  // Calculate prior on invariant fraction
  if (not fixed(2)) {
    double i = parameters_[2];
    Pr *= beta_pdf(i,0.01,200);
  }

  return Pr;
}

indel::PairHMM NewIndelModel::get_branch_HMM(double t) const 
{
  using namespace states;

  if (not time_dependant)
    t = 1;

  double rate    = exp(parameters_[0]);
  double e = exp(parameters_[1]);
  double i = parameters_[2];

  // (1-e) * delta / (1-delta) = P(indel)
  // But move the (1-e) into the RATE to make things work
  double mu = rate*t/(1.0-e);
  double P_indel = 1.0 - exp(-mu);
  double A = P_indel*(1.0-i);
  double delta = A/(1+A);

  if (1 - 2*delta <0)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e >= 1)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta > 0 and delta <= 1);
  assert(e > 0 and e <= 1);
  
  indel::PairHMM Q;

  Q(S ,S ) = 0;
  Q(S ,M ) = 1 - 2*delta;
  Q(S ,G1) = delta;
  Q(S ,G2) = delta;
  Q(S ,E)  = 1;

  Q(M ,S ) = 1-e;
  Q(M ,M ) = e;
  Q(M ,G1) = 0;
  Q(M ,G2) = 0;
  Q(M ,E)  = 0;

  Q(G1,S ) = 1-e;
  Q(G1,M ) = 0;
  Q(G1,G1) = e;
  Q(G1,G2) = 0;
  Q(G1,E ) = 0;

  Q(G2,S ) = 1-e;
  Q(G2,M ) = 0;
  Q(G2,G1) = 0;
  Q(G2,G2) = e;
  Q(G2,E ) = 0;

  Q(E, S ) = 0;
  Q(E ,M ) = 0;
  Q(E ,G1) = 0;
  Q(E ,G2) = 0;
  Q(E ,E ) = 1;

  remove_one_state(Q,S);

  // This is a bit of a hack...
  Q(S ,S ) = 0;
  Q(S ,M ) = 1;
  Q(S ,G1) = 0;
  Q(S ,G2) = 0;
  Q(S ,E)  = 0;

  return Q;
}

string NewIndelModel::name() const {
  string s = "fragment-based indels ";
  
  if (time_dependant)
    s += "+ T ";
  s += "[HMM]";
  return s;
}

string NewIndelModel::parameter_name(int i) const {
  if (i==0)
    return "NEW::lambda";
  else if (i==1)
    return "NEW::epsilon";
  else if (i==2)
    return "NEW::invariant";
  else if (i==3)
    return "NEW::prior_median";
  else if (i==4)
    return "NEW::prior_stddev";
  else if (i==5)
    return "NEW::prior_length";
  else
    return i_parameter_name(i,6);
}

efloat_t NewIndelModel::lengthp(int l) const {
  return 1;
}

NewIndelModel::NewIndelModel(bool b)
  :IndelModel(6),time_dependant(b)
{
  parameters_[0] = -5;
  parameters_[1] = -0.5;
  parameters_[2] = 0.1;
  parameters_[3] = -5;
  parameters_[4] = 0.5;
  parameters_[5] = 5.0;

  recalc();
}
