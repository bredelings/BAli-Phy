#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/computation.H"
#include "sequence/alphabet.H"
#include "computation/expression/expression.H"

extern "C" closure builtin_function_alphabetSize(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);

    if (not arg.is_a<alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg.print()<<" is not an alphabet.";

    const alphabet& a = arg.as_<alphabet>();
    return {a.n_letters()};
}

extern "C" closure builtin_function_alphabet_letters(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);
    if (not arg.is_a<alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg.print()<<" is not an alphabet.";

    const alphabet& a = arg.as_<alphabet>();

    auto v = new EVector;
    for(int i=0;i<a.n_letters();i++)
	v->push_back(String(a.letter(i)));
  
    return v;
}

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    if (a.is_a<Triplets>())
	return Args.evaluate(0).as_<Triplets>().getNucleotides();
    else
	throw myexception()<<"getNucleotides: object "<<a.print()<<" is not a Triplets alphabet.";
}

extern "C" closure builtin_function_getAminoAcids(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    if (a.is_a<Codons>())
	return Args.evaluate(0).as_<Codons>().getAminoAcids();
    else
	throw myexception()<<"getAminoAcids: object "<<a.print()<<" is not a Codons alphabet.";
}

extern "C" closure builtin_function_triplets(OperationArgs& Args)
{
    auto nuc  = Args.evaluate(0);

    if (nuc.is_a<Nucleotides>())
	return {Triplets(nuc.as_<Nucleotides>())};
    else
	throw myexception()<<"triplets: object "<<nuc.print()<<" is not a Nucleotides alphabet.";
}

extern "C" closure builtin_function_codons(OperationArgs& Args)
{
    auto nuc  = Args.evaluate(0);
    auto code = Args.evaluate(1);

    if (not nuc.is_a<Nucleotides>())
	throw myexception()<<"codons: object "<<nuc.print()<<"is not a Nucleotides alphabet.";

    if (not code.is_a<Genetic_Code>())
	throw myexception()<<"codons: object "<<code.print()<<"is not a Genetic_Code object.";

    return {Codons(nuc.as_<Nucleotides>(), AminoAcids(), code.as_<Genetic_Code>())};
}

extern "C" closure builtin_function_genetic_code_standard(OperationArgs&)
{
    return {new Standard_Genetic_Code()};
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

    if (C.is_a<Codons>())
	return {C.as_<Codons>().translate(codon)};
    else
	throw myexception()<<"translate: object "<<C.print()<<" is not a Codons alphabet.";
}

