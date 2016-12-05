module Alphabet where
{
builtin getNucleotides 1 "getNucleotides" "Alphabet";
builtin getAminoAcids 1 "getAminoAcids" "Alphabet";
builtin alphabetSize 1 "alphabetSize" "Alphabet";
builtin builtin_alphabet_letters 1 "alphabet_letters" "Alphabet";
builtin translate 2 "translate" "Alphabet";
builtin triplets 1 "triplets" "Alphabet";
builtin codons 2 "codons" "Alphabet";
builtin builtin_dna 1 "dna" "Alphabet";
builtin builtin_rna 1 "rna" "Alphabet";
builtin builtin_aa  1 "aa" "Alphabet";
builtin builtin_aaWithStop 1 "aaWithStop" "Alphabet";

dna = builtin_dna ();
rna = builtin_rna ();
aa  = builtin_aa  ();
aaWithStop = builtin_aaWithStop ();

alphabet_letters a = map listFromString (list_from_vector (builtin_alphabet_letters a) )
}
