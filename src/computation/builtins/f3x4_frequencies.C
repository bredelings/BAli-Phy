#include "computation/computation.H"
#include "sequence/alphabet.H"

using std::vector;

extern "C" closure builtin_function_f3x4_frequencies(OperationArgs& Args)
{
  auto T = Args.evaluate_as<Triplets>(0);
  // The way alphabet is currently implemented, triplets must be triplets of nucleotides.

  auto pi1_ = Args.evaluate_as<Vector<double>>(1);
  const vector<double>& pi1 = pi1_->t;

  auto pi2_ = Args.evaluate_as<Vector<double>>(2);
  const vector<double>& pi2 = pi2_->t;

  auto pi3_ = Args.evaluate_as<Vector<double>>(3);
  const vector<double>& pi3 = pi3_->t;

  Vector<double> pi;
  pi.t.resize(T->size());
  for(int i=0;i<T->size();i++)
    pi.t[i] = pi1[T->sub_nuc(i,0)] * pi2[T->sub_nuc(i,1)] * pi3[T->sub_nuc(i,2)];

  // Some triplets may be missing from the triplet alphabet (e.g. stop codons).  So renormalize.

  double scale = 1.0/sum(pi.t);
  for(double& d : pi.t)
    d *= scale;

  assert(std::abs(sum(pi.t) - 1.0) < 1.0e-9);

  return pi;
}
