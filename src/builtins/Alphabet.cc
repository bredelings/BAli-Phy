#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "builtins/native-vector-input.hh"
#include "computation/machine/args.hh"
#include "sequence/alphabet.hh"
#include "sequence/ambiguity.hh"
#include "sequence/doublets.hh"
#include "sequence/RNAEdits.hh"
#include "sequence/codons.hh"
#include "util/string/sanitize.hh"

using Alphabet = PtrBox<alphabet>;
using Ambiguities = Box<ambiguity_database>;
using std::vector;

extern "C" R::Exp simple_function_alphabetSize(vector<R::Exp>& args)
{
    auto arg = get_arg(args);

    if (not arg.is_a<Alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg.print()<<" is not an alphabet.";

    const alphabet& a = *arg.as_<Alphabet>();
    return a.n_letters();
}

extern "C" closure builtin_function_alphabet_letters(OperationArgs& Args)
{
    auto arg = Args.evaluate_slot_to_value(0);
    if (not arg.is_a<Alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg.print()<<" is not an alphabet.";

    const alphabet& a = *arg.as_<Alphabet>();

    auto v = new R::RVector;
    for(int i=0;i<a.n_letters();i++)
	v->push_back(a.letter(i));
  
    return v;
}

extern "C" R::Exp simple_function_find_letter(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    if (not arg0.is_a<Alphabet>())
	throw myexception()<<"alphabetSize: object "<<arg0.print()<<" is not an alphabet.";

    const alphabet& a = *arg0.as_<Alphabet>();

    auto arg1 = get_arg(args);
    auto& letter = arg1.as_string();

    auto state = a.find_letter(letter);
    if (not state)
        throw myexception()<<"Alphabet '"<<a.name<<"' doesn't contain letter '"<<sanitize_string(letter)<<"'.";
    return *state;
}

extern "C" closure builtin_function_getNucleotides(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto t = arg0.poly_cast<alphabet,Triplets>())
	return Alphabet(t->getNucleotides().clone());
    if (auto d = arg0.poly_cast<alphabet,Doublets>())
	return Alphabet(d->getNucleotides().clone());
    if (auto e = arg0.poly_cast<alphabet,RNAEdits>())
	return Alphabet(e->getNucleotides().clone());
    else
	throw myexception()<<"getNucleotides: object "<<a.print()<<" is not a Doublets or Triplets alphabet.";
}

extern "C" closure builtin_function_getAminoAcids(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto c = arg0.poly_cast<alphabet,Codons>())
	return Alphabet(c->getAminoAcids().clone());
    else
	throw myexception()<<"getAminoAcids: object "<<a.print()<<" is not a Codons alphabet.";
}

extern "C" closure builtin_function_mkDoublets(OperationArgs& Args)
{
    auto arg0  = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto n = arg0.poly_cast<alphabet,Nucleotides>())
	return Alphabet(new Doublets(*n));
    else
	throw myexception()<<"mkDoublets: object "<<a.print()<<" is not a Nucleotides alphabet.";
}

extern "C" closure builtin_function_mkRNAEdits(OperationArgs& Args)
{
    auto arg0  = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    if (auto n = arg0.poly_cast<alphabet,Nucleotides>())
	return Alphabet(new RNAEdits(*n));
    else
	throw myexception()<<"mkRNAEdits: object "<<a.print()<<" is not a Nucleotides alphabet.";
}

extern "C" closure builtin_function_mkTriplets(OperationArgs& Args)
{
    auto arg0  = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    if (auto n = arg0.poly_cast<alphabet,Nucleotides>())
	return Alphabet(new Triplets(*n));
    else
	throw myexception()<<"mkTriplets: object "<<a.print()<<" is not a Nucleotides alphabet.";
}

extern "C" closure builtin_function_mkCodons(OperationArgs& Args)
{
    auto arg0  = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();
    auto nuc = dynamic_cast<const Nucleotides*>(&a);
    if (not nuc)
	throw myexception()<<"mkCodons: object "<<a.print()<<"is not a Nucleotides alphabet.";

    auto arg1 = Args.evaluate_slot_to_value(1);
    if (not arg1.is_a<Box<Genetic_Code>>())
	throw myexception()<<"mkCodons: object "<<arg1.print()<<"is not a Genetic_Code object.";
    const auto& code = arg1.as_<Box<Genetic_Code>>();

    return Alphabet(new Codons(*nuc, AminoAcids(), code));
}

extern "C" closure builtin_function_geneticCodeByNumber(OperationArgs& Args)
{
    int number = Args.evaluate_slot_to_value(0).as_int();
    return Box<Genetic_Code>(get_genetic_code(number));
}

extern "C" closure builtin_function_geneticCodeRaw(OperationArgs& Args)
{
    auto name = Args.evaluate_slot_to_value(0).as_string();
    return Box<Genetic_Code>(get_genetic_code(name));
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

extern "C" closure builtin_function_mkNumeric(OperationArgs& Args)
{
    int n = Args.evaluate_slot_to_value(0).as_int();

    return Alphabet(new Numeric(n));
}

extern "C" R::Exp simple_function_translate(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    const alphabet& a = *arg0.as_<Alphabet>();

    int codon = get_arg(args).as_int();

    if (auto c = dynamic_cast<const Codons*>(&a))
	return c->translate(codon);
    else
	throw myexception()<<"translate: object "<<a.print()<<" is not a Codons alphabet.";
}

// Translate an encoded observation using the ambiguity database that owns its negative codes.
extern "C" R::Exp simple_function_translateObservation(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    const alphabet& a = *arg0.as_<Alphabet>();
    auto arg1 = get_arg(args);
    auto& ambiguities = arg1.as_<Ambiguities>();
    int codon = get_arg(args).as_int();

    if (auto c = dynamic_cast<const Codons*>(&a))
        return c->translate_observation(codon, ambiguities);
    else
        throw myexception()<<"translateObservation: object "<<a.print()<<" is not a Codons alphabet.";
}

extern "C" closure builtin_function_sequenceToTextRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& a = *arg0.as_checked<Alphabet>();
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& ambiguities = arg1.as_<Ambiguities>();

    auto letter_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 2, "Alphabet.sequenceToTextRaw");
    auto letter_sequence = letter_input.view();

    std::string text;

    int widened = 0;
    for(int letter: letter_sequence)
    {
        auto [spelling, exact] = ambiguities.decode(a, letter);
        text += spelling;
        widened += not exact;
    }

    if (widened)
        std::cerr<<"Warning: widened "<<widened<<" non-Cartesian or unspellable ambiguities while writing sequences.\n";

    return text;
}
