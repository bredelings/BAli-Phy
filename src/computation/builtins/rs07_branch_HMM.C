#include "computation/computation.H"
#include "sequence/alphabet.H"
#include "dp/2way.H"
#include "imodel/imodel.H"

using std::vector;

extern "C" closure builtin_function_rs07_branch_HMM(OperationArgs& Args)
{
  double e = *Args.evaluate_as<Double>(0);
  double D = *Args.evaluate_as<Double>(1);
  double heat = *Args.evaluate_as<Double>(2);
  constructor in_training_c = *Args.evaluate_as<constructor>(3);
  bool in_training = true;
  if (in_training_c.f_name == "Prelude.False")
    in_training = false;

  using namespace A2::states;

  // Here D = rate * t

  // Return a model with all probabilities zero if e==1.
  // Scaling time by 1/(1.0-e) doesn't work if e==1.
  if (e >= 1)
    return indel::PairHMM();

  // (1-e) * delta / (1-delta) = P(indel)
  // But move the (1-e) into the RATE to make things work
  double mu = D/(1.0-e);
  double P_indel = 1.0 - exp(-mu);
  double A = P_indel;

  if (in_training) A = std::min(A,0.005);

  double delta = A/(1+A);

  // Note: If the branch is disconnected, then t < -0.5
  //  if (t < -0.5) delta = 0.5;

  double f = 0.1; //unaligned fraction
  delta = pow(delta, heat) * pow(f/(1+f),1-heat);
  e = 1.0 - pow(1.0 - e, heat);

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

  //  if (t < -0.5)  then don't fragmentize

  // turn the model into a fragment model
  fragmentize(Q,e);

  remove_one_state(Q,S);

  Q.start_pi(S)  = 0;
  Q.start_pi(M)  = 1;
  Q.start_pi(G1) = 0;
  Q.start_pi(G2) = 0;
  Q.start_pi(E)  = 0;

  return Q;
}
