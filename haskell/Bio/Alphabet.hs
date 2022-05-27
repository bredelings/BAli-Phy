module Bio.Alphabet where

data Alphabet

builtin getNucleotides 1 "Alphabet:getNucleotides"

builtin getAminoAcids 1 "Alphabet:getAminoAcids"

builtin alphabetSize 1 "Alphabet:alphabetSize"

builtin builtin_letters 1 "Alphabet:alphabet_letters"
letters a = map listFromString (list_from_vector (builtin_letters a) )

builtin builtin_find_letter 2 "Alphabet:find_letter"
find_letter a letter = builtin_find_letter a (list_to_string letter)

builtin translate 2 "Alphabet:translate"

builtin doublets 1 "Alphabet:doublets"

builtin triplets 1 "Alphabet:triplets"

builtin codons 2 "Alphabet:codons"

builtin builtin_dna 1 "Alphabet:dna"
dna = builtin_dna ()

builtin builtin_rna 1 "Alphabet:rna"
rna = builtin_rna ()

builtin builtin_aa  1 "Alphabet:aa"
aa  = builtin_aa  ()
amino_acids = builtin_aa

builtin builtin_aaWithStop 1 "Alphabet:aaWithStop"
aaWithStop = builtin_aaWithStop ()
amino_acids_with_stop = builtin_aaWithStop ()

builtin builtin_standard_code 1 "Alphabet:genetic_code_standard"
standard_code = builtin_standard_code ()

