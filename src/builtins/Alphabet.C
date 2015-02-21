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

  object_ptr<OVector> v(new OVector);
  for(int i=0;i<a.n_letters();i++)
    v->push_back(new String(a.letter(i)));
  
  return v;
}

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
  object_ptr<const Triplets> T = Args.evaluate_as<Triplets>(0);
  const Nucleotides& N = T->getNucleotides();

  return N;
}

extern "C" closure builtin_function_amino_acids_for_codons(OperationArgs& Args)
{
  object_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
  object_ptr<OVector> v(new OVector);
  for(int codon=0;codon<C->size();codon++)
    v->push_back(Int(C->translate(codon)));

  return v;
}
