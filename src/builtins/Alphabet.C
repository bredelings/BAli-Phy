#include "computation/computation.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_alphabetSize(OperationArgs& Args)
{
  const alphabet& a = Args.evaluate(0).as_<alphabet>();

  return {a.n_letters()};
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

extern "C" closure builtin_function_triplets(OperationArgs& Args)
{
    auto nuc  = Args.evaluate(0);

    return {Triplets(nuc.as_<Nucleotides>())};
}

extern "C" closure builtin_function_codons(OperationArgs& Args)
{
    auto nuc  = Args.evaluate(0);
    auto aa   = Args.evaluate(1);

//    auto code = Args.evaluate(2).as_<Genetic_Code>();
    return {Codons(nuc.as_<Nucleotides>(), aa.as_<AminoAcids>(), Standard_Genetic_Code())};
}

extern "C" closure builtin_function_dna(OperationArgs&)
{
    return {DNA()};
}

extern "C" closure builtin_function_rna(OperationArgs&)
{
    return {RNA()};
}

extern "C" closure builtin_function_aa(OperationArgs&)
{
    return {AminoAcids()};
}

extern "C" closure builtin_function_aaWithStop(OperationArgs&)
{
    return {AminoAcidsWithStop()};
}

extern "C" closure builtin_function_translate(OperationArgs& Args)
{
  auto C = Args.evaluate(0);
  int codon = Args.evaluate(1).as_int();

  return {C.as_<Codons>().translate(codon)};
}
