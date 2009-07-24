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

  //------------------------------------------------------------------------//

  bool PairTransducer::is_match(int i)  const {return e1[i]>=0 and e2[i]>=0;}
  bool PairTransducer::is_insert(int i) const {return e1[i]< 0 and e2[i]>=0;}
  bool PairTransducer::is_delete(int i) const {return e1[i]>=0 and e2[i]< 0;}
  bool PairTransducer::is_silent(int i) const {return e1[i]< 0 and e2[i]< 0;}
  bool PairTransducer::is_start(int i)  const {return i == start_;}
  bool PairTransducer::is_end(int i)    const {return i == end_;}

  // find a set of non-silent states (along with start and end) to use instead
  void PairTransducer::remove_silent()
  {
    // construct the new set of states, and mark the (single) start and (single) end
    int new_start = -1;
    int new_end = -1;
    vector<int> keep;
    for(int i=0;i<n_states();i++)
      if (is_start(i) or is_end(i) or not is_silent(i)) {
	if (is_start(i))
	  new_start = keep.size();
	if (is_end(i))
	  new_end = keep.size();
	keep.push_back(i);
      }
      else
	remove_one_state(*this,i);

    // This only works because we already REMOVED the other states
    Matrix Q(keep.size(),keep.size());
    for(int i=0;i<Q.size1();i++)
      for(int j=0;j<Q.size2();j++)
	Q(i,j) = (*this)(keep[i],keep[j]);
    
    // only keep the emission information for states we are going to keep
    e1 = apply_indices(e1,keep);
    e2 = apply_indices(e2,keep);

    // swap in the new start and end
    start_ = new_start;
    end_ = new_end;

    // swap in the new transition probs
    this->swap(Q);

    for(int i=0;i<n_states();i++)
      assert(is_start(i) or is_end(i) or not is_silent(i));
  }

  void PairTransducer::check_states() 
  {
    Matrix& Q = *this;

    // store a copy of our transition probabilities to restore at the end
    Matrix temp = Q;

    vector<int> from;

    // Collect states that emit in Seq1 and remove others
    for(int i=0;i<n_states();i++) 
    {
      if (is_end(i))
	continue;
      else if (is_start(i) or e1[i] >= 0)
	from.push_back(i);
      else
	remove_one_state(*this,i);
    }

    // Collect all sets of states which have transition probability of 1.0
    vector< vector<int> > to;
    for(int l=0;l<n_letters();l++) {
      to.push_back(vector<int>());
      for(int i=0;i<n_states();i++)
	if (e1[i] == l)
	  to.back().push_back(i);
    }
    to.push_back(vector<int>(1,end_state()));

    // Check transitions from each reading state j to each letter i
    for(int i=0;i<to.size();i++)
      for(int j=0;j<from.size();j++)
      {
	double total = 0;
	for(int k=0;k<to[i].size();k++)
	  total += Q(from[j],to[i][k]);

	if (std::abs(total - 1.0) > 1.0e-9) {
	  std::cerr<<"Transition from state "<<j<<" to letter "<<i<<" was "<<total<<", not 1.0!"<<endl;
	  abort();
	}
	  
      }

    // restore the saved copy of our transition probabilities
    Q = temp;
  }

  PairTransducer::PairTransducer(int s,int e,const vector<int>& v1, const vector<int>& v2)
    :Matrix(v1.size(),v1.size()),e1(v1),e2(v2),start_(s),end_(e)
  {
    assert(e1.size() == n_states());
    assert(e2.size() == n_states());

    n_letters_ = 1+std::max(max(v1),max(v2));

    for(int i=0;i<size1();i++)
      for(int j=0;j<size2();j++)
	(*this)(i,j) = 0;
  }
}

string i_parameter_name(int i,int n) {
  if (i>=n)
    throw myexception()<<"substitution model: refered to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return string("pI") + convertToString(i);
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

string SimpleIndelModel::name() const {return "RS05";}

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
  double log_epsilon = parameter(1);
  double E_length = log_epsilon - logdiff(0,log_epsilon);
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
  string s = "RS07";
  
  if (not time_dependant)
    s += "[-T]";
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

TransducerIndelModel::~TransducerIndelModel() {}

efloat_t TKF1_Transducer::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(parameter(0),parameter(2), parameter(3));

  // Calculate prior on mean sequence length
  Pr *= exponential_pdf(parameter(1), parameter(4));

  return Pr;
}

// States: S, letters, E
Matrix TKF1_Transducer::root_chain() const
{
  double lambda = exp(parameter(0));
  double mean_length = parameter(1);
  double sigma = mean_length/(1.0 + mean_length); // E L = s/(1-s)
  double mu = lambda/sigma;                       // s = lambda/mu

  Matrix M(3,3);

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M(i,j) = 0;

  M(0,1) = lambda/mu;
  M(0,2) = 1 - M(0,1);

  M(1,1) = lambda/mu;
  M(1,2) = 1 - M(1,1);

  return M;
}

// lambda is the insertion rate.
// mu     is the deletion  rate.
indel::PairTransducer get_TKF1_Transducer(double t,double lambda, double mu)
{
  const int S = 0;
  const int M = 1;
  const int D = 2;
  const int I = 3;
  const int E = 4;
  const int W = 6;

  vector<int> e1(7);
  e1[S] = -1;
  e1[M] =  0;
  e1[D] =  0;
  e1[I] = -1;
  e1[E] = -1;
  e1[5] = -1;
  e1[W] = -1;

  vector<int> e2(7);
  e2[S] = -1;
  e2[M] =  0;
  e2[D] = -1;
  e2[I] =  0;
  e2[E] = -1;
  e2[5] = -1;
  e2[W] = -1;

  indel::PairTransducer Q(0,4,e1,e2);

  double U = exp(-mu*t);
  double B = (1.0 - exp((lambda-mu)*t))/(mu - lambda*exp((lambda - mu)*t));

  Q(S ,5 ) = 1;

  Q(M, 5)  = 1.0;

  Q(D, W)  = mu*B/(1.0 - U);
  Q(D, I)  = 1.0 - Q(D,W);

  Q(I, I)  = lambda*B;
  Q(I, W)  = 1.0 - Q(I,I);

  Q(5, I ) = lambda * B;
  Q(5, W ) = 1.0 - Q(5,I);

  Q(W, M ) = U;
  Q(W, D ) = 1.0 - U;
  Q(W, E ) = 1.0;


  Q.remove_silent();

  Q.check_states();

  return Q;
  
}

indel::PairTransducer TKF1_Transducer::get_branch_Transducer(double t) const 
{
  if (not time_dependent)
    t = 1;

  double lambda = exp(parameter(0));
  double mean_length = parameter(1);
  double sigma = mean_length/(1.0 + mean_length); // E L = s/(1-s)
  double mu = lambda/sigma;                       // s = lambda/mu

  assert(lambda < mu);

  return get_TKF1_Transducer(t,lambda,mu);
}

string TKF1_Transducer::name() const 
{
  return "TKF1_Transducer";
}

TKF1_Transducer::TKF1_Transducer(bool b)
  :time_dependent(b)
{
  add_parameter("lambda",-5);
  add_parameter("mean_length",100);
  add_parameter("lambda::prior_median", -5);
  add_parameter("lambda::prior_stddev", 1.5);
  add_parameter("mean_length::prior_mean", 1.5);
}


// States: S, letters, E
Matrix FS_Transducer::root_chain() const
{
  double tau      = parameter(6);
  double mean_s   = parameter(4);
  double mean_f   = parameter(5);

  double e_s = mean_s/(1+mean_s);
  double e_f = mean_f/(1+mean_f);

  Matrix M(4,4);

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M(i,j) = 0;

  M(0,1) = 0.5;
  M(0,2) = 0.5;

  M(1,1) = e_s/(1 - tau*tau*(1-e_s)*(1-e_f) );
  M(1,2) = tau*(1-e_s)*e_f/(1 - tau*tau*(1-e_s)*(1-e_f) );
  M(1,3) = 1 - M(1,1) - M(1,2);

  M(2,1) = tau*(1-e_f)*e_s/(1 - tau*tau*(1-e_f)*(1-e_s) );
  M(2,2) = e_f/(1 - tau*tau*(1-e_f)*(1-e_s) );
  M(2,3) = 1 - M(2,1) - M(2,2);

  return M;
}

efloat_t FS_Transducer::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(parameter(0), parameter(7), parameter(9));
  Pr *= laplace_pdf(parameter(1), parameter(8), parameter(9));

  // Calculate prior on r_s
  double E_length_mean = parameter(9);

  double log_r_s = parameter(2);
  double E_length_r_s = log_r_s - logdiff(0,log_r_s);
  Pr *= exp_exponential_pdf(E_length_r_s,E_length_mean);

  double log_r_f = parameter(3);
  double E_length_r_f = log_r_f - logdiff(0,log_r_f);
  Pr *= exp_exponential_pdf(E_length_r_f ,E_length_mean);

  // Calculate prior on mean sequence length
  //  Pr *= exponential_pdf(parameter(2), parameter(4));
  //  Pr *= exponential_pdf(parameter(3), parameter(5));

  return Pr;
}


// This transducer does NOT allow S/S -/F -/F S/S
// The transition to the end state is also rather hacked in.

// lambda is the insertion rate.
// mu     is the deletion  rate.
indel::PairTransducer get_FS_Transducer(double t,double delta_s,double delta_f, double r_s, double r_f, double tau)
{
  const int S = 0;

  const int Ms  = 1;
  const int Ds  = 2;
  const int IsS = 3;
  const int IsF = 4;
  const int Ws  = 5;
  const int WsD = 6;
  const int TsS = 7;
  const int TsF = 8;
  const int E   = 9;

  const int Mf  = 10;
  const int Df  = 11;
  const int IfF = 12;
  const int IfS = 13;
  const int Wf  = 14;
  const int WfD = 15;
  const int TfF = 16;
  const int TfS = 17;
  // 15

  const int TsE = 18;
  const int IsE = 19;
  const int TfE = 20;
  const int IfE = 21;

  vector<int> e1(22, -1);
  vector<int> e2(22, -1);

  // matches
  e1[Ms]  =  e2[Ms]  =  0;
  e1[Mf]  =  e2[Mf]  =  1;

  // deletes
  e1[Ds]  =  0;
  e1[Df]  =  1;

  // inserts - conditional on the next letter in the input sequence
  e2[IsS] =  0;
  e2[IsF] =  0;
  e2[IsE] =  0;

  e2[IfS] =  1;
  e2[IfF] =  1;
  e2[IfE] =  1;

  indel::PairTransducer Q(S,E,e1,e2);

  Q(S,TsS)    = 1;
  Q(S,TfF)    = 1;
  Q(S,TfE)    = 1; // this is NOT L/R symmetric or F/S symmetric

  /*************************************************/

  // Once you've hit state TsS, the next state (in seq 1) will be an S
  // Except for the link-outs (below) these probs should all sum to 1

  Q(TsS,IsS)  = delta_s;
  Q(TsS,Ws)   = (1 - delta_s)*(1 - tau*tau);
  Q(TsS,TfS)  = (1 - delta_s)*tau*tau;

  Q(IsS,IsS)  = r_s;
  Q(IsS,TsS)  = 1 - r_s;

  Q(TfS,IfS)  = delta_f;
  Q(TfS,TsS)  = 1 - delta_f;
  
  Q(Ws,Ms)    = (1-2*delta_s)/(1-delta_s);
  Q(Ws,Ds)    = 1 - Q(Ws,Ms);

  Q(Ms,TsS)   = 1;

  Q(Ds,TsS)   = 1 - r_s;
  Q(Ds,WsD)   = r_s;

  Q(WsD,Ds)   = 1;

  Q(IsF,IsF)  = r_s;
  Q(IsF,TsF)  = 1 - r_s;

  
  //  This is different than coming from the START state
  //  because we are conditioning on the previous state being an S.
  Q(Ms,TsF)   = Q(Ds,TsF) = 1;    // Link to path out of S into F
  Q(Ms,TsE)   = Q(Ds,TsE) = 1;    // Link to path out of S into E

  /*************************************************/

  // Once you've hit state TsF, the next state (in seq 1) will be an F
  // Except for the link-outs (below) these probs should all sum to 1

  Q(TfF,IfF)  = delta_f;
  Q(TfF,Wf)   = (1 - delta_f)*(1 - tau*tau);
  Q(TfF,TsF)  = (1 - delta_f)*tau*tau;

  Q(IfF,IfF)  = r_f;
  Q(IfF,TfF)  = 1 - r_f;

  Q(TsF,IsF)  = delta_s;
  Q(TsF,TfF)  = 1 - delta_s;

  Q(Wf,Mf)    = (1-2*delta_f)/(1-delta_f);
  Q(Wf,Df)    = 1 - Q(Wf,Mf);

  Q(Mf,TfF)   = 1;

  Q(Df,TfF)   = 1 - r_f;
  Q(Df,WfD)   = r_f;

  Q(WfD,Df)   = 1;

  Q(IfS,IfS)  = r_f;
  Q(IfS,TfS)  = 1 - r_f;

  //  This is different than coming from the START state
  //  because we are conditioning on the previous state being an S.
  Q(Mf,TfS)   = Q(Df,TfS) = 1;    // Link to path out of F into S
  Q(Mf,TfE)   = Q(Df,TfE) = 1;    // Link to path out of F into E



  /*************************************************/

  // This may not be the best idea...

  Q(TsE,IsE) = delta_s;
  Q(TsE,E)   = (1 - delta_s)*(1 - tau*tau);
  Q(TsE,TfE) = (1 - delta_s)*tau*tau;

  Q(IsE,IsE) = r_s;
  Q(IsE,TsE) = 1 - r_s;

  Q(TfE,IfE) = delta_f;
  Q(TfE,E)   = (1 - delta_f)*(1 - tau*tau);
  Q(TfE,TfE) = (1 - delta_f)*tau*tau;

  Q(IfE,IfE) = r_f;
  Q(IfE,TfE) = 1 - r_f;


  Q.remove_silent();

  Q.check_states();

  return Q;
}

indel::PairTransducer FS_Transducer::get_branch_Transducer(double t) const 
{
  if (not time_dependent)
    t = 1;

  double lambda_s = exp(parameter(0));
  double lambda_f = exp(parameter(1));
  double r_s      = exp(parameter(2));
  double r_f      = exp(parameter(3));
  double mean_length_s = parameter(4);
  double mean_length_f = parameter(5);
  double tau      = parameter(6);

  double sigma_s = mean_length_s/(1.0 + mean_length_s); // E L = s/(1-s)
  double mu_s = lambda_s/sigma_s;                       // s = lambda/mu

  double sigma_f = mean_length_f/(1.0 + mean_length_f); // E L = s/(1-s)
  double mu_f = lambda_f/sigma_f;                       // s = lambda/mu

  assert(lambda_s < mu_s);
  assert(lambda_f < mu_f);

  double A_s = lambda_s*t/(1.0 - r_s);
  double B_s = 1.0 - exp(-A_s);
  double delta_s = B_s/(1+B_s);

  double A_f = lambda_f*t/(1.0 - r_f);
  double B_f = 1.0 - exp(-A_f);
  double delta_f = B_f/(1+B_f);

  return get_FS_Transducer(t,delta_s,delta_f,r_s,r_f,tau);
}

string FS_Transducer::name() const 
{
  return "FS_Transducer";
}

FS_Transducer::FS_Transducer(bool b)
  :time_dependent(b)
{
  add_parameter("lambda_s", -5);
  add_parameter("lambda_f", -3);
  add_parameter("r_s", -0.5);
  add_parameter("r_f", -0.3);
  add_parameter("mean_length_s", 20);
  add_parameter("mean_length_f", 20);
  add_parameter("switch", 0.01);
  add_parameter("lambda::prior_median_s", -5);
  add_parameter("lambda::prior_median_f", -3);
  add_parameter("lambda::prior_stddev", 1.5);
  add_parameter("mean_length::prior_mean", 1.5);
}
