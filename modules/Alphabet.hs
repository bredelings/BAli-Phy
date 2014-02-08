module Alphabet where
{
builtin getNucleotides 1 "getNucleotides";
builtin alphabetSize 1 "alphabetSize";
builtin builtin_alphabet_letters 1 "alphabet_letters" "alphabetSize";

alphabet_letters a = map listFromString (list_from_vector (builtin_alphabet_letters a) )
}
