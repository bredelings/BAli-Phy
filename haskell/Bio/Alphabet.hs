module Bio.Alphabet where

builtin getNucleotides 1 "getNucleotides" "Alphabet"

builtin getAminoAcids 1 "getAminoAcids" "Alphabet"

builtin alphabetSize 1 "alphabetSize" "Alphabet"

builtin builtin_letters 1 "alphabet_letters" "Alphabet"
letters a = map listFromString (list_from_vector (builtin_letters a) )

builtin translate 2 "translate" "Alphabet"

builtin doublets 1 "doublets" "Alphabet"

builtin triplets 1 "triplets" "Alphabet"

builtin codons 2 "codons" "Alphabet"

builtin builtin_dna 1 "dna" "Alphabet"
dna = builtin_dna ()

builtin builtin_rna 1 "rna" "Alphabet"
rna = builtin_rna ()

builtin builtin_aa  1 "aa" "Alphabet"
aa  = builtin_aa  ()
amino_acids = builtin_aa

builtin builtin_aaWithStop 1 "aaWithStop" "Alphabet"
aaWithStop = builtin_aaWithStop ()
amino_acids_with_stop = builtin_aaWithStop ()

builtin builtin_standard_code 1 "genetic_code_standard" "Alphabet"
standard_code = builtin_standard_code ()

