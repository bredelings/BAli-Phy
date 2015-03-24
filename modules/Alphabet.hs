module Alphabet where
{
builtin getNucleotides 1 "getNucleotides" "Alphabet";
builtin getAminoAcids 1 "getAminoAcids" "Alphabet";
builtin alphabetSize 1 "alphabetSize" "Alphabet";
builtin builtin_alphabet_letters 1 "alphabet_letters" "Alphabet";
builtin translate 2 "translate" "Alphabet";

alphabet_letters a = map listFromString (list_from_vector (builtin_alphabet_letters a) )
}
