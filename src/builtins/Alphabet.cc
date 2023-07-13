#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"

using Alphabet = PtrBox<alphabet>;

extern "C" closure builtin_function_alphabetSize(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);

    if (not arg.is_a<Alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg.print()<<" is not an alphabet.";

    const alphabet& a = *arg.as_<Alphabet>();
    return {a.n_letters()};
}

extern "C" closure builtin_function_alphabet_letters(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);
    if (not arg.is_a<Alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg.print()<<" is not an alphabet.";

    const alphabet& a = *arg.as_<Alphabet>();

    auto v = new EVector;
    for(int i=0;i<a.n_letters();i++)
	v->push_back(String(a.letter(i)));
  
    return v;
}

extern "C" closure builtin_function_find_letter(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    if (not arg0.is_a<Alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg0.print()<<" is not an alphabet.";

    const alphabet& a = *arg0.as_<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& letter = arg1.as_<String>();

    int index = a.find_letter(letter);

    return {index};
}

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto t = arg0.poly_cast<alphabet,Triplets>())
	return Alphabet(t->getNucleotides().clone());
    if (auto d = arg0.poly_cast<alphabet,Doublets>())
	return Alphabet(d->getNucleotides().clone());
    else
	throw myexception()<<"getNucleotides: object "<<a.print()<<" is not a Doublets or Triplets alphabet.";
}

extern "C" closure builtin_function_getAminoAcids(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto c = arg0.poly_cast<alphabet,Codons>())
	return Alphabet(c->getAminoAcids().clone());
    else
	throw myexception()<<"getAminoAcids: object "<<a.print()<<" is not a Codons alphabet.";
}

extern "C" closure builtin_function_doublets(OperationArgs& Args)
{
    auto arg0  = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto n = arg0.poly_cast<alphabet,Nucleotides>())
	return Alphabet(new Doublets(*n));
    else
	throw myexception()<<"doublets: object "<<a.print()<<" is not a Nucleotides alphabet.";
}

extern "C" closure builtin_function_triplets(OperationArgs& Args)
{
    auto arg0  = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    if (auto n = arg0.poly_cast<alphabet,Nucleotides>())
	return Alphabet(new Triplets(*n));
    else
	throw myexception()<<"triplets: object "<<a.print()<<" is not a Nucleotides alphabet.";
}

extern "C" closure builtin_function_codons(OperationArgs& Args)
{
    auto arg0  = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    auto nuc = dynamic_cast<const Nucleotides*>(&a);
    if (not nuc)
	throw myexception()<<"codons: object "<<a.print()<<"is not a Nucleotides alphabet.";

    auto arg1 = Args.evaluate(1);
    if (not arg1.is_a<PtrBox<Genetic_Code>>())
	throw myexception()<<"codons: object "<<arg1.print()<<"is not a Genetic_Code object.";
    const Genetic_Code& code = *arg1.as_<PtrBox<Genetic_Code>>();

    return Alphabet(new Codons(*nuc, AminoAcids(), code));
}

extern "C" closure builtin_function_genetic_code_standard(OperationArgs&)
{
    return PtrBox<Genetic_Code>(new Standard_Genetic_Code());
}

extern "C" closure builtin_function_dna(OperationArgs&)
{
    return Alphabet(new DNA());
}

extern "C" closure builtin_function_rna(OperationArgs&)
{
    return Alphabet(new RNA());
}

extern "C" closure builtin_function_aa(OperationArgs&)
{
    return Alphabet(new AminoAcids());
}

extern "C" closure builtin_function_aaWithStop(OperationArgs&)
{
    return Alphabet(new AminoAcidsWithStop());
}

extern "C" closure builtin_function_translate(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    int codon = Args.evaluate(1).as_int();

    if (auto c = dynamic_cast<const Codons*>(&a))
	return {c->translate(codon)};
    else
	throw myexception()<<"translate: object "<<a.print()<<" is not a Codons alphabet.";
}

extern "C" closure builtin_function_sequenceToTextRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& letter_sequence = arg1.as_<EVector>();

    auto result = object_ptr<String>(new String);
    auto& text = *result;

    for(auto& letter: letter_sequence)
    {
        int l = letter.as_int();
        text += a.lookup(l);
    }

    return result;
}

