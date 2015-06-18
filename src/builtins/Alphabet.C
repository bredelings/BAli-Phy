#include "computation/computation.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_alphabetSize(OperationArgs& Args)
{
  const alphabet& a = Args.evaluate(0).as_<alphabet>();

  return Int(a.n_letters());
}

extern "C" closure builtin_function_alphabet_letters(OperationArgs& Args)
{
  const alphabet& a = Args.evaluate(0).as_<alphabet>();

  auto v = new EVector;
  for(int i=0;i<a.n_letters();i++)
    v->push_back(String(a.letter(i)));
  
  return v;
}

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
  return Args.evaluate(0).as_<Triplets>().getNucleotides();
}

extern "C" closure builtin_function_getAminoAcids(OperationArgs& Args)
{
  return Args.evaluate(0).as_<Codons>().getAminoAcids();
}

extern "C" closure builtin_function_translate(OperationArgs& Args)
{
  auto C = Args.evaluate(0);
  int codon = Args.evaluate(1).as_int();

  return {C.as_<Codons>().translate(codon)};
}
