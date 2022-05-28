module Bio.Alphabet where

data Alphabet

foreign import bpcall "Alphabet:getNucleotides" getNucleotides 1

foreign import bpcall "Alphabet:getAminoAcids" getAminoAcids 1

foreign import bpcall "Alphabet:alphabetSize" alphabetSize 1

foreign import bpcall "Alphabet:alphabet_letters" builtin_letters 1
letters a = map listFromString (list_from_vector (builtin_letters a) )

foreign import bpcall "Alphabet:find_letter" builtin_find_letter 2
find_letter a letter = builtin_find_letter a (list_to_string letter)

foreign import bpcall "Alphabet:translate" translate 2

foreign import bpcall "Alphabet:doublets" doublets 1

foreign import bpcall "Alphabet:triplets" triplets 1

foreign import bpcall "Alphabet:codons" codons 2

foreign import bpcall "Alphabet:dna" builtin_dna 1
dna = builtin_dna ()

foreign import bpcall "Alphabet:rna" builtin_rna 1
rna = builtin_rna ()

foreign import bpcall "Alphabet:aa" builtin_aa  1
aa  = builtin_aa  ()
amino_acids = builtin_aa

foreign import bpcall "Alphabet:aaWithStop" builtin_aaWithStop 1
aaWithStop = builtin_aaWithStop ()
amino_acids_with_stop = builtin_aaWithStop ()

foreign import bpcall "Alphabet:genetic_code_standard" builtin_standard_code 1
standard_code = builtin_standard_code ()

