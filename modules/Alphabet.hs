module Alphabet where
{
builtin getNucleotides 1 "getNucleotides" "Alphabet";
builtin alphabetSize 1 "alphabetSize" "Alphabet";
builtin builtin_alphabet_letters 1 "alphabet_letters" "Alphabet";
builtin builtin_amino_acids_for_codons 1 "amino_acids_for_codons" "Alphabet";

alphabet_letters a = map listFromString (list_from_vector (builtin_alphabet_letters a) );

amino_acids_for_codons a = list_from_vector (builtin_amino_acids_for_codons a)
}
