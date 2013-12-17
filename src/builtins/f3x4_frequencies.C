#include "computation/computation.H"
#include "sequence/alphabet.H"

using std::vector;

extern "C" closure builtin_function_f3x4_frequencies(OperationArgs& Args)
{
  auto T = Args.evaluate_as<Triplets>(0);
  // The way alphabet is currently implemented, triplets must be triplets of nucleotides.

  auto pi1 = Args.evaluate_as<Vector<double>>(1);

  auto pi2 = Args.evaluate_as<Vector<double>>(2);

  auto pi3 = Args.evaluate_as<Vector<double>>(3);

  Vector<double> pi;
  pi.resize(T->size());
  for(int i=0;i<T->size();i++)
    pi[i] = (*pi1)[T->sub_nuc(i,0)] * (*pi2)[T->sub_nuc(i,1)] * (*pi3)[T->sub_nuc(i,2)];

  // Some triplets may be missing from the triplet alphabet (e.g. stop codons).  So renormalize.

  double scale = 1.0/sum(pi);
  for(double& d : pi)
    d *= scale;

  assert(std::abs(sum(pi) - 1.0) < 1.0e-9);

  return pi;
}
