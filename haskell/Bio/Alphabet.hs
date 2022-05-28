module Bio.Alphabet where

data Alphabet

builtin "Alphabet:getNucleotides" getNucleotides 1

builtin "Alphabet:getAminoAcids" getAminoAcids 1

builtin "Alphabet:alphabetSize" alphabetSize 1

builtin "Alphabet:alphabet_letters" builtin_letters 1
letters a = map listFromString (list_from_vector (builtin_letters a) )

builtin "Alphabet:find_letter" builtin_find_letter 2
find_letter a letter = builtin_find_letter a (list_to_string letter)

builtin "Alphabet:translate" translate 2

builtin "Alphabet:doublets" doublets 1

builtin "Alphabet:triplets" triplets 1

builtin "Alphabet:codons" codons 2

builtin "Alphabet:dna" builtin_dna 1
dna = builtin_dna ()

builtin "Alphabet:rna" builtin_rna 1
rna = builtin_rna ()

builtin "Alphabet:aa" builtin_aa  1
aa  = builtin_aa  ()
amino_acids = builtin_aa

builtin "Alphabet:aaWithStop" builtin_aaWithStop 1
aaWithStop = builtin_aaWithStop ()
amino_acids_with_stop = builtin_aaWithStop ()

builtin "Alphabet:genetic_code_standard" builtin_standard_code 1
standard_code = builtin_standard_code ()

