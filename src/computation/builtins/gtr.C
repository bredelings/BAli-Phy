#include "computation/computation.H"
#include "mytypes.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_gtr(OperationArgs& Args)
{
  object_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
  double AG = *Args.evaluate_as<Double>(1);
  double AT = *Args.evaluate_as<Double>(2);
  double AC = *Args.evaluate_as<Double>(3);
  double GT = *Args.evaluate_as<Double>(4);
  double GC = *Args.evaluate_as<Double>(5);
  double TC = *Args.evaluate_as<Double>(6);

  assert(N->size()==4);

  object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

  R->t.resize(N->size());

  double total = AG + AT + AC + GT + GC + TC;

  R->t(0,1) = AG/total;
  R->t(0,2) = AT/total;
  R->t(0,3) = AC/total;

  R->t(1,2) = GT/total;
  R->t(1,3) = GC/total;

  R->t(2,3) = TC/total;

  return R;
}
