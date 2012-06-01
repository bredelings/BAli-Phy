/*
   Copyright (C) 2004-2008 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file imodel.H
///
/// \brief This file implements classes and functions related to insertions and deletions.
///

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
using std::string;
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
	  std::cerr<<"Transition from state "<<j<<" to letter "<<i<<" was "<<total<<", not 1.0!"<<std::endl;
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

    // set all entries in the transition matrix to zero.
    clear();
  }
}

void IndelModel::set_training(bool b)
{
  in_training = b;
}

bool IndelModel::is_training() const
{
  return in_training;
}

double IndelModel::get_heat() const
{
  return heat;
}

void IndelModel::set_heat(double h)
{
  assert(0<= h and h <= 1);
  heat = h;
}

IndelModel::IndelModel()
  :in_training(false), heat(1)
{ }

IndelModel::~IndelModel() {}


// Get a bitmask of the list of possible next letters emitted in sequence 1 after state @s0
// The mask is indexed by letter1, letter2, ... , E
vector<int> get_possible_next_letters(int s0, const indel::PairTransducer& PTM)
{
  vector<int> visited(PTM.n_states(),0);

  vector<int> next;
  next.push_back(s0);

  vector<int> possible(PTM.n_letters()+1,0);

  while (next.size())
  {
    // states to consider next time around
    vector<int> next2;

    // For each next (unvisited) letter s1
    for(int i=0;i<next.size();i++)
    {
      int s1 = next[i];

      if (visited[s1]) continue;

      visited[s1] = 1;

      // For each unvisited neighbor s2 of s1
      for(int s2=0;s2<PTM.n_states();s2++)
      {
	if (PTM(s1,s2) > 0) 
	{
	  // If it emits a letter in sequence1, then that letter is possible;
	  if (PTM.emits_1(s2) != -1) {
	    possible[PTM.emits_1(s2)] = 1;
	    visited[s2] = 1;
	  }
          // If it is the end state, then the end state is possible
	  else if (s2 == PTM.end_state()) {
	    possible.back() = 1;
	    visited[s2] = 1;
	  }
          // If is silent in sequence 1 and not the end state, the consider
          // that state's neighbors.
	  else if (not visited[s2])
	    next2.push_back(s2);
	}
      }
    }

    next = next2;
  }

  return possible;
}

transducer_state_info::transducer_state_info(const indel::PairTransducer& PTM)
  :M(PTM.n_letters()+1,PTM.n_letters()+1),
   D(PTM.n_letters()+1,PTM.n_letters()+1),
   I(PTM.n_letters()+1,PTM.n_letters()+1)
{
  // Find possible next letters
  vector< vector<int> > possible;
  for(int i=0;i<PTM.n_states();i++)
    possible.push_back(get_possible_next_letters(i,PTM));

  // Find unique states S[i,j] of type Si that next emit j in sequence 1
  const int o = PTM.n_letters()+1;

  for(int i=0;i<o;i++)
    for(int j=0;j<o;j++)
      M(i,j) = D(i,j) = I(i,j) = -1;

  for(int i=0;i<PTM.n_states();i++)
  {
    int t1 = PTM.emits_1(i);
    if (t1 == -1)
      t1 = PTM.emits_2(i);
    if (t1 == -1)
      continue;

    for(int t2=0;t2<PTM.n_letters()+1;t2++) 
      if (possible[i][t2]) {

	if (PTM.is_match(i)) {
	  assert(M(t1,t2) == -1);
	  M(t1,t2) = i;
	}
	if (PTM.is_delete(i)) {
	  assert(D(t1,t2) == -1);
	  D(t1,t2) = i;
	}
	if (PTM.is_insert(i)) {
	  assert(I(t1,t2) == -1);
	  I(t1,t2) = i;
	}
      }
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

indel::PairHMM SimpleIndelModel::get_branch_HMM(double) const 
{
  using namespace states;

  double delta   = exp(get_parameter_value_as<Double>(0));
  double e       = exp(get_parameter_value_as<Double>(1));
  double t       = exp(get_parameter_value_as<Double>(2));

  if (is_training()) delta = std::min(delta,0.005);

  // Return a model with all probabilities zero if e==1.
  if (e >= 1)
    return indel::PairHMM();

  if (t < -0.5)
    delta = 0.5;
  else
  {
    double f = 0.1; //unaligned fraction
    delta = pow(delta, get_heat()) * pow(f/(1+f),1-get_heat());
    e = 1.0 - pow(1.0 - e, get_heat());
  }

  if (delta > 0.5)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e > 1.0)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;

  assert(delta >= 0.0 and delta <= 1.0);
  assert(e >= 0.0 and e < 1.0);

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
  double lambda_O = get_parameter_value_as<Double>(0);
  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel)) - log(D);

  Pr *= laplace_pdf(rate,-5, 0.5);

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double lambda_E = get_parameter_value_as<Double>(1);
  if (lambda_E >= 0) return 0;

  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = 5.0;

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  return Pr;
}

string SimpleIndelModel::name() const {return "RS05";}

SimpleIndelModel::SimpleIndelModel()
  :QE(Q1.size1(),Q1.size2())
{
  add_parameter(Parameter("delta",  Double(-5), upper_bound(-log(2))));
  add_parameter(Parameter("epsilon",Double(-0.25))); // no upper bound on transformed scale
  add_parameter(Parameter("tau",    Double(log(0.001)), upper_bound(0)));

  recalc_all();
}

efloat_t NewIndelModel::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda_O
  double rate = get_parameter_value_as<Double>(0);

  Pr *= laplace_pdf(rate,get_parameter_value_as<Double>(2), get_parameter_value_as<Double>(3));

  // Calculate prior on lambda_E - shouldn't depend on lambda_O
  double log_epsilon = get_parameter_value_as<Double>(1);

  // We can't scale time appropriately if e = 1.
  // Also, the calculation below diverges if e = 1.
  if (log_epsilon >= 0) return 0;

  double E_length = log_epsilon - logdiff(0,log_epsilon);
  double E_length_mean = get_parameter_value_as<Double>(4);

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  return Pr;
}

indel::PairHMM NewIndelModel::get_branch_HMM(double t) const 
{
  using namespace states;

  if (not time_dependant)
    t = 1;

  double rate    = exp(get_parameter_value_as<Double>(0));
  double e = exp(get_parameter_value_as<Double>(1));

  // Return a model with all probabilities zero if e==1.
  // Scaling time by 1/(1.0-e) doesn't work if e==1.
  if (e >= 1)
    return indel::PairHMM();

  // (1-e) * delta / (1-delta) = P(indel)
  // But move the (1-e) into the RATE to make things work
  double mu = rate*t/(1.0-e);
  double P_indel = 1.0 - exp(-mu);
  double A = P_indel;

  if (is_training()) A = std::min(A,0.005);

  double delta = A/(1+A);

  if (t < -0.5)
    delta = 0.5;
  else
  {
    double f = 0.1; //unaligned fraction
    delta = pow(delta, get_heat()) * pow(f/(1+f),1-get_heat());
    e = 1.0 - pow(1.0 - e, get_heat());
  }

  if (1 - 2*delta <0)
    throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

  if (e > 1)
    throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
  assert(delta >= 0 and delta <= 1);
  assert(e >= 0 and e < 1);

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
  double e = exp(get_parameter_value_as<Double>(1));
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
  add_parameter(Parameter("lambda",   Double(-4)));
  add_parameter(Parameter("epsilon",  Double(-0.25))); // no upper bound on transformed scale
  add_parameter(Parameter("lambda:prior_median", Double(-4)));
  add_parameter(Parameter("lambda:prior_stddev", Double(1)));
  add_parameter(Parameter("epsilon:prior_length", Double(10)));
}


efloat_t TKF1::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(get_parameter_value_as<Double>(0),get_parameter_value_as<Double>(2), get_parameter_value_as<Double>(3));

  // Calculate prior on mean sequence length
  Pr *= exponential_pdf(get_parameter_value_as<Double>(1), get_parameter_value_as<Double>(4));

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

  double lambda = exp(get_parameter_value_as<Double>(0));
  double mean_length = get_parameter_value_as<Double>(1);
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
  double mean_length = get_parameter_value_as<Double>(1);

  double sigma = mean_length/(1.0 + mean_length);

  return (1.0-sigma)*pow(efloat_t(sigma),l);
}

TKF1::TKF1(bool b)
  :time_dependant(b)
{
  add_parameter(Parameter("lambda",Double(-5)));
  add_parameter(Parameter("mean_length",Double(100)));
  add_parameter(Parameter("lambda:prior_median", Double(-5)));
  add_parameter(Parameter("lambda:prior_stddev", Double(1.5)));
  add_parameter(Parameter("mean_length:prior_mean", Double(1.5)));
}


efloat_t TKF2::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(get_parameter_value_as<Double>(0),get_parameter_value_as<Double>(3), get_parameter_value_as<Double>(4));

  // Calculate prior on epsilon
  double lambda_E = get_parameter_value_as<Double>(1);
  double E_length = lambda_E - logdiff(0,lambda_E);
  double E_length_mean = get_parameter_value_as<Double>(5);

  Pr *= exp_exponential_pdf(E_length,E_length_mean);

  // Calculate prior on mean sequence length
  Pr *= exponential_pdf(get_parameter_value_as<Double>(2), get_parameter_value_as<Double>(6));

  return Pr;
}

indel::PairHMM TKF2::get_branch_HMM(double t) const 
{
  using namespace states;

  if (not time_dependant)
    t = 1;

  double lambda = exp(get_parameter_value_as<Double>(0));
  double e = exp(get_parameter_value_as<Double>(1));
  double mean_length = get_parameter_value_as<Double>(2);
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
  double mean_length = get_parameter_value_as<Double>(1);

  double sigma = mean_length/(1.0 + mean_length);

  return (1.0-sigma)*pow(efloat_t(sigma),l);
}

TKF2::TKF2(bool b)
  :time_dependant(b)
{
  add_parameter(Parameter("lambda",Double(-5)));
  add_parameter(Parameter("epsilon",Double(-0.25), upper_bound(0)));
  add_parameter(Parameter("mean_length",Double(100)));
  add_parameter(Parameter("lambda:prior_median", Double(-5)));
  add_parameter(Parameter("lambda:prior_stddev", Double(1.5)));
  add_parameter(Parameter("epsilon:prior_length", Double(10)));
  add_parameter(Parameter("mean_length:prior_mean", Double(1.5)));
}

TransducerIndelModel::~TransducerIndelModel() {}

efloat_t TKF1_Transducer::prior() const 
{
  efloat_t Pr = 1;

  // Calculate prior on lambda
  Pr *= laplace_pdf(get_parameter_value_as<Double>(0),get_parameter_value_as<Double>(2), get_parameter_value_as<Double>(3));

  // Calculate prior on mean sequence length
  Pr *= exponential_pdf(get_parameter_value_as<Double>(1), get_parameter_value_as<Double>(4));

  return Pr;
}

// States: S, letters, E
Matrix TKF1_Transducer::root_chain() const
{
  double lambda = exp(get_parameter_value_as<Double>(0));
  double mean_length = get_parameter_value_as<Double>(1);
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

#ifndef NDEBUG
  Q.check_states();
#endif

  return Q;
  
}

indel::PairTransducer TKF1_Transducer::get_branch_Transducer(double t) const 
{
  if (not time_dependent)
    t = 1;

  double lambda = exp(get_parameter_value_as<Double>(0));
  double mean_length = get_parameter_value_as<Double>(1);
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
  add_parameter(Parameter("lambda",Double(-5), upper_bound(0)));
  add_parameter(Parameter("mean_length",Double(100)));
  add_parameter(Parameter("lambda:prior_median", Double(-5)));
  add_parameter(Parameter("lambda:prior_stddev", Double(1.5)));
  add_parameter(Parameter("mean_length:prior_mean", Double(1.5)));
}


// States: S, letters, E
Matrix FS_Transducer::root_chain() const
{
  double tau      = get_parameter_value_as<Double>(5);
  double mean_s   = get_parameter_value_as<Double>(3);
  double mean_f   = get_parameter_value_as<Double>(4);

  double e_s = mean_s/(1+mean_s);
  double e_f = mean_f/(1+mean_f);

  Matrix M(4,4);

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M(i,j) = 0;

  M(0,1) = mean_s/(mean_s+mean_f);
  M(0,2) = mean_f/(mean_s+mean_f);

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

  // Calculate prior on indel rate
  double lambda_s = get_parameter_value_as<Double>(0);
  double lambda_f = get_parameter_value_as<Double>(1);

  if (lambda_f < lambda_s) return 0;

  Pr *= laplace_pdf(lambda_s, get_parameter_value_as<Double>(6), get_parameter_value_as<Double>(8));
  Pr *= laplace_pdf(lambda_f, get_parameter_value_as<Double>(7), get_parameter_value_as<Double>(8));

  // Calculate prior on indel length
  double E_length_mean = get_parameter_value_as<Double>(9);
  double log_r = get_parameter_value_as<Double>(2);
  double E_length_r = log_r - logdiff(0,log_r);

  Pr *= exp_exponential_pdf(E_length_r,E_length_mean);

  // uniform prior on switch

  return Pr;
}


// Currently, multiple insertions in a row are not allowed, 
// even though multiple deletions are.  This is because I
// would have to figure out a way not to keep track of the LEFT
// end of the insertion also.
// 
// -- Um... couldn't we just have the (1-r) simply go to Rij where 
//    i is the type of residue we last inserted?

// Insertions into FF, FS and SF are delta_f, insertions into SS are delta_s

// Deletions delete only one type (no mixed-type deletions) and are 
//  independent of their left and right neighbors.
// (Currently, we model deletions as occuring at the first letter
//  or the deletion.  The deletion may continue until the first time
//  it either choose to terminate OR sees a letter of another type.
//  Thus, the rate of deletions depends on how close other types are.)

indel::PairTransducer get_FS_Transducer(double /* t */,double delta_s,double delta_f, double r, double Rsf, double Rfs)
{
  double Rss = 1.0-Rsf;
  double Rff = 1.0-Rfs;

  const int S = 0;    // 0

  const int Ms  = 1;  // 1
  const int Ds  = 2;  // 2
  const int IsS = 3;  // 3
  const int IsF = 4;  // 4
  const int Ws  = 5;
  const int WsD = 6;
  const int TsS = 7;
  const int TsF = 8;
  const int E   = 9;  // 5

  const int Mf  = 10; // 6
  const int Df  = 11; // 7
  const int IfF = 12; // 8
  const int IfS = 13; // 9
  const int Wf  = 14;
  const int WfD = 15;
  const int TfF = 16;
  const int TfS = 17;

  const int TsE = 18;
  const int IsE = 19; // 10
  const int TfE = 20;
  const int IfE = 21; // 11

  // Rij means that, having emitted an @i, we will now go on to emit a @j
  const int RSS = 22;
  const int RSF = 23;
  const int RSE = 24;
  const int RFS = 25;
  const int RFF = 26;
  const int RFE = 27;


  vector<int> e1(28, -1);
  vector<int> e2(28, -1);

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

 // This is not F/S symmetric: it treats start like an F

  Q(S,RFS)    = 1;
  Q(S,RFF)    = 1;
  Q(S,RFE)    = 1;

  /*************************************************/

  // Except for the link-outs (below) these probs should all sum to 1

  Q(Ws,Ms)    = (1-2*delta_s)/(1-delta_s);
  Q(Ws,Ds)    = 1 - Q(Ws,Ms);

  Q(Ms,RSS)   = 1;

  Q(Ds,RSS)   = 1 - r;
  Q(Ds,WsD)   = r;

  Q(WsD,Ds)   = 1;

  Q(RSS,Ws)   = 1-delta_s;
  Q(RSS,TsS)  = delta_s;      // Pr(Insertion between two SS)

  // given that the last column was an S, and the next letter in seq1 is an S, do we insert S or F?
  Q(TsS,IsS)  = Rss*Rss/(Rss*Rss+Rsf*Rfs);
  Q(TsS,IfS)  = Rsf*Rfs/(Rss*Rss+Rsf*Rfs);

  // given that the last column was an S, and the next letter in seq1 is an S, do we insert S or F?
  Q(TfS,IsS)  = Rfs*Rss/(Rfs*Rss+Rff*Rfs);
  Q(TfS,IfS)  = Rff*Rfs/(Rfs*Rss+Rff*Rfs);
  
  Q(IsS,TsS)  = r;            // go back and insert another letter .. of undecided type.
  Q(IsS,Ws)   = 1 - r;        // we are done inserting

  Q(IfS,TfS)  = r;
  Q(IfS,Ws)   = 1 - r;

  
  //  This is different than coming from the START state
  //  because we are conditioning on the previous state being an S.
  Q(Ms,RSF)   = Q(Ds,RSF) = 1;    // Link to path out of S into F
  Q(Ms,RSE)   = Q(Ds,RSE) = 1;    // Link to path out of S into E

  Q(RSF,TsF)  = delta_f;
  Q(RSF,Wf)   = 1-delta_f;

  Q(RSE,TsE)  = delta_f;
  Q(RSE,E)    = 1-delta_f;

  /*************************************************/

  // Except for the link-outs (below) these probs should all sum to 1

  Q(Wf,Mf)    = (1-2*delta_f)/(1-delta_f);
  Q(Wf,Df)    = 1 - Q(Wf,Mf);

  Q(Mf,RFF)   = 1;

  Q(Df,RFF)   = 1 - r;
  Q(Df,WfD)   = r;

  Q(WfD,Df)   = 1;

  Q(RFF,Wf)   = 1-delta_f;
  Q(RFF,TfF)  = delta_f;      // Pr(Insertion between two SS)

  // given that the last column was an S, and the next letter in seq1 is an F, do we insert S or F?
  Q(TsF,IsF)  = Rss*Rsf/(Rss*Rsf+Rsf*Rff);
  Q(TsF,IfF)  = Rsf*Rff/(Rss*Rsf+Rsf*Rff);

  // given that the last column was an S, and the next letter in seq1 is an F, do we insert S or F?
  Q(TfF,IsF)  = Rfs*Rsf/(Rfs*Rsf+Rff*Rff);
  Q(TfF,IfF)  = Rff*Rff/(Rfs*Rsf+Rff*Rff);
  
  Q(IsF,TsF)  = r;            // go back and insert another letter .. of undecided type.
  Q(IsF,Wf)   = 1 - r;        // we are done inserting

  Q(IfF,TfF)  = r;
  Q(IfF,Wf)   = 1 - r;

  
  //  This is different than coming from the START state
  //  because we are conditioning on the previous state being an S.
  Q(Mf,RFS)   = Q(Df,RFS) = 1;    // Link to path out of F into S
  Q(Mf,RFE)   = Q(Df,RFE) = 1;    // Link to path out of F into E

  Q(RFS,TfS)  = delta_f;
  Q(RFS,Ws)   = 1-delta_f;

  Q(RFE,TfE)  = delta_f;
  Q(RFE,E)    = 1-delta_f;

  /*************************************************/

  // given that the last column was an S, and the next letter in seq1 is an E (F), do we insert S or F?
  Q(TsE,IsE)  = Rss*Rsf/(Rss*Rsf+Rsf*Rff);
  Q(TsE,IfE)  = Rsf*Rff/(Rss*Rsf+Rsf*Rff);

  // given that the last column was an S, and the next letter in seq1 is an E (F), do we insert S or F?
  Q(TfE,IsE)  = Rfs*Rsf/(Rfs*Rsf+Rff*Rff);
  Q(TfE,IfE)  = Rff*Rff/(Rfs*Rsf+Rff*Rff);
  
  Q(IsE,TsE)  = r;            // go back and insert another letter .. of undecided type.
  Q(IsE,E)    = 1 - r;        // we are done inserting

  Q(IfE,TfE)  = r;
  Q(IfE,E)    = 1 - r;


  Q.remove_silent();

#ifndef NDEBUG
  Q.check_states();
#endif

  return Q;
}

indel::PairTransducer FS_Transducer::get_branch_Transducer(double t) const 
{
  if (not time_dependent)
    t = 1;

  double lambda_s      = exp(get_parameter_value_as<Double>(0));
  double lambda_f      = exp(get_parameter_value_as<Double>(1));
  double r             = exp(get_parameter_value_as<Double>(2));
  double mean_length_s = get_parameter_value_as<Double>(3);
  double mean_length_f = get_parameter_value_as<Double>(4);
  //  double tau           = get_parameter_value_as<Double>(5);

  double sigma_s = mean_length_s/(1.0 + mean_length_s); // E L = s/(1-s)
  double mu_s = lambda_s/sigma_s;                       // s = lambda/mu

  double sigma_f = mean_length_f/(1.0 + mean_length_f); // E L = s/(1-s)
  double mu_f = lambda_f/sigma_f;                       // s = lambda/mu

  assert(lambda_s < mu_s);
  assert(lambda_f < mu_f);

  double A_s = lambda_s*t;
  double B_s = 1.0 - exp(-A_s);
  double delta_s = B_s/(1+B_s);

  double A_f = lambda_f*t;
  double B_f = 1.0 - exp(-A_f);
  double delta_f = B_f/(1+B_f);

  double Rsf = -1;
  double Rfs = -1;
  {
    Matrix R = root_chain();
    //    int start = 0;
    int S     = 1;
    int F     = 2;
    int end   = 3;
    Rsf = R(S,F)/(1-R(S,end));
    Rfs = R(F,S)/(1-R(F,end));
  }

  return get_FS_Transducer(t, delta_s, delta_f, r, Rsf, Rfs);
}

string FS_Transducer::name() const 
{
  return "FS_Transducer";
}

FS_Transducer::FS_Transducer(bool b)
  :time_dependent(b)
{
  add_parameter(Parameter("lambda_s", Double(-5), upper_bound(0)));                  // 0
  add_parameter(Parameter("lambda_f", Double(-3), upper_bound(0)));                  // 1
  add_parameter(Parameter("r", Double(-0.3), upper_bound(0)));                       // 2
  add_parameter(Parameter("mean_length_s", Double(20), lower_bound(0)));             // 3
  add_parameter(Parameter("mean_length_f", Double(20), lower_bound(0)));             // 4
  add_parameter(Parameter("switch", Double(0.1), between(0,1)));                   // 5
  add_parameter(Parameter("lambda:prior_median_s", Double(-5)));    // 6
  add_parameter(Parameter("lambda:prior_median_f", Double(-3)));    // 7
  add_parameter(Parameter("lambda:prior_stddev",Double( 1.5)));     // 8
  add_parameter(Parameter("mean_length:prior_mean", Double(1.5)));  // 9
}

#include <bitset>
// Question: can we still fake start states?
// We need a collection of non-silent states such that
// \sum_i P(i,j) = P(Start,j)

// multiple hidden-markov model
typedef std::bitset<64> bitmask_t;

class mhmm
{
public:
  vector<bitmask_t> state_emit;
  int start;
  int end;

  Matrix Q;

  bitmask_t all_bits;

  int n_characters() const
  {
    return all_bits.count();
  }

  void remap_bits(const vector<int>& map)
  {
    assert(map.size() == n_characters());
    int B = map.size();

    vector<bitmask_t> state_emit2(state_emit.size());
    for(int i=0;i<state_emit.size();i++)
    {
      bitmask_t mask;
      for(int j=0;j<B;j++)
	mask.set(map[j],state_emit[i].test(j));
      state_emit2[i] = mask;
    }

    std::swap(state_emit, state_emit2);
  }

  //  mhmm(const vector<bitmask_t>&, const vector<double>&, const Matrix&, double);
};

mhmm remap_bits(const mhmm& m1, const vector<int>& map)
{
  mhmm m2 = m1;
  m2.remap_bits(map);
  return m2;
}

// s:s (m/i):(m/d) (m,i,s)_r:I D:(m,d,e)_c e:e
mhmm Glue(const mhmm& top, int out, mhmm bottom, int in)
{
  // 1. Classify top states into S, E, M/I, and D
  vector<int> m_or_i1;
  vector<int> d1;

  for(int i=0;i<top.state_emit.size();i++)
  {
    if (i == top.start or i == top.end) continue;

    assert(top.state_emit[i].any());

    if (top.state_emit[i].test(out))
      m_or_i1.push_back(i);
    else
      d1.push_back(i);
  }

  // 2. Classify bottom states into S, E, M/I, and D
  vector<int> m_or_d2;
  vector<int> i2;

  for(int i=0;i<bottom.state_emit.size();i++)
  {
    if (i == bottom.start or i == bottom.end) continue;

    assert(bottom.state_emit[i].any());

    if (bottom.state_emit[i].test(in))
      m_or_d2.push_back(i);
    else
      i2.push_back(i);
  }

  // 3. Map top & bottom bits onto the bits of the output mhmm
  vector<int> map (bottom.n_characters());
  int temp = top.n_characters();
  for(int i=0;i<map.size();i++)
  {
    if (i == in)
      map[i] = out;
    else
      map[i] = temp++;
  }
  bottom.remap_bits(map);

  // 4. Construct states for the new HMM
  enum status_t {active, remembered, committed};

  struct parts
  {
    int state1;
    status_t status1;
    int state2;
    status_t status2;
  };

  mhmm G;
  vector<parts> state_parts;

  // M/I:M/D
  for(int s1: m_or_i1)
    for(int s2: m_or_d2)
    {
      G.state_emit.push_back(top.state_emit[s1] | bottom.state_emit[s2]);
      state_parts.push_back({s1,active,s2,active});
    }

  // How do we record the info on the hidden states?
  // Actually, this is the hardest part of the whole thing!

  /*
  // (M,I,S)_r:I
  for(int s1: m_or_i1)
    for(int s2: i2)
    {
      G.state_emit.push_back(bottom.state_emit[s2]);
      state_parts.push_back({s1,remembered,s2,active});
    }
  */

  for(int s2: i2)
  {
    G.state_emit.push_back(bottom.state_emit[s2]);
    state_parts.push_back({top.start, remembered,s2,active});
  }

  for(int s1: d1)
    for(int s2: m_or_d2)
    {
      G.state_emit.push_back(top.state_emit[s1]);
      state_parts.push_back({s1, active,s2,committed});
    }

  for(int s1: d1)
  {
    G.state_emit.push_back(top.state_emit[s1]);
    state_parts.push_back({s1, active, bottom.end, committed});
  }

  // e:e
  G.end = G.state_emit.size();
  G.state_emit.push_back(bitmask_t());
  state_parts.push_back({top.end, active, bottom.end, active});

  // s:s
  G.start = G.state_emit.size();
  G.state_emit.push_back(bitmask_t());
  state_parts.push_back({top.start, active, bottom.start, active});

  // The D/* -> */I transition is forbidded by the fact that D is never remembered and I is never committed.

  // 5. Compute transition matrix
  G.Q.resize( G.state_emit.size(), G.state_emit.size() );
  for(int i=0;i<G.state_emit.size();i++)
    for(int j=0;j<G.state_emit.size();j++)
    {
      double p = 1;
      status_t si1 = state_parts[i].status1;
      status_t si2 = state_parts[i].status2;
      status_t sj1 = state_parts[j].status1;
      status_t sj2 = state_parts[j].status2;
      if (sj1 == active)
	p = top.Q(state_parts[i].state1,state_parts[j].state1);
      else if (sj1 == remembered and state_parts[i].state1 != state_parts[j].state1)
	p = 0;

      if (si1 == active)
	p *= bottom.Q(state_parts[i].state2,state_parts[j].state2);
      else if (si2 == committed and state_parts[i].state2 != state_parts[j].state2)
	p *= 0;
    }

  // 6. Find a 

  return G;
}
