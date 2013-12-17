#include "computation/computation.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
  object_ptr<const Triplets> T = Args.evaluate_as<Triplets>(0);
  const Nucleotides& N = T->getNucleotides();

  return N;
}
