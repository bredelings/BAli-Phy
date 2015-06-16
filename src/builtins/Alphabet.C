#include "computation/computation.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_alphabetSize(OperationArgs& Args)
{
  const alphabet& a = *Args.evaluate_as<alphabet>(0);

  return Int(a.n_letters());
}

extern "C" closure builtin_function_alphabet_letters(OperationArgs& Args)
{
  const alphabet& a = *Args.evaluate_as<alphabet>(0);

  object_ptr<EVector> v(new EVector);
  for(int i=0;i<a.n_letters();i++)
    v->push_back(String(a.letter(i)));
  
  return v;
}

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
  object_ptr<const Triplets> T = Args.evaluate_as<Triplets>(0);
  const Nucleotides& N = T->getNucleotides();

  return N;
}

extern "C" closure builtin_function_getAminoAcids(OperationArgs& Args)
{
  object_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
  const AminoAcids& A = C->getAminoAcids();

  return A;
}

extern "C" closure builtin_function_translate(OperationArgs& Args)
{
  object_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
  int codon = *Args.evaluate_as<Int>(1);

  Int amino_acid = C->translate(codon);

  return amino_acid;
}
