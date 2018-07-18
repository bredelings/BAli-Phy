module Alphabet where
{
builtin getNucleotides 1 "getNucleotides" "Alphabet";

builtin getAminoAcids 1 "getAminoAcids" "Alphabet";

builtin alphabetSize 1 "alphabetSize" "Alphabet";

builtin builtin_alphabet_letters 1 "alphabet_letters" "Alphabet";
alphabet_letters a = map listFromString (list_from_vector (builtin_alphabet_letters a) );

builtin translate 2 "translate" "Alphabet";

builtin triplets 1 "triplets" "Alphabet";

builtin codons 2 "codons" "Alphabet";

builtin builtin_dna 1 "dna" "Alphabet";
dna = builtin_dna ();
        
builtin builtin_rna 1 "rna" "Alphabet";
rna = builtin_rna ();
        
builtin builtin_aa  1 "aa" "Alphabet";
aa  = builtin_aa  ();

builtin builtin_aaWithStop 1 "aaWithStop" "Alphabet";
aaWithStop = builtin_aaWithStop ();

builtin builtin_standard_code 1 "genetic_code_standard" "Alphabet";
standard_code = builtin_standard_code ();
}
