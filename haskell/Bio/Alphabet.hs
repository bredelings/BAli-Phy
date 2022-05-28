module Bio.Alphabet where

data Alphabet

foreign import bpcall "Alphabet:getNucleotides" getNucleotides :: () -> ()

foreign import bpcall "Alphabet:getAminoAcids" getAminoAcids :: () -> ()

foreign import bpcall "Alphabet:alphabetSize" alphabetSize :: () -> ()

foreign import bpcall "Alphabet:alphabet_letters" builtin_letters :: () -> ()
letters a = map listFromString (list_from_vector (builtin_letters a) )

foreign import bpcall "Alphabet:find_letter" builtin_find_letter :: () -> () -> ()
find_letter a letter = builtin_find_letter a (list_to_string letter)

foreign import bpcall "Alphabet:translate" translate :: () -> () -> ()

foreign import bpcall "Alphabet:doublets" doublets :: () -> ()

foreign import bpcall "Alphabet:triplets" triplets :: () -> ()

foreign import bpcall "Alphabet:codons" codons :: () -> () -> ()

foreign import bpcall "Alphabet:dna" builtin_dna :: () -> ()
dna = builtin_dna ()

foreign import bpcall "Alphabet:rna" builtin_rna :: () -> ()
rna = builtin_rna ()

foreign import bpcall "Alphabet:aa" builtin_aa  :: () -> ()
aa  = builtin_aa  ()
amino_acids = builtin_aa

foreign import bpcall "Alphabet:aaWithStop" builtin_aaWithStop :: () -> ()
aaWithStop = builtin_aaWithStop ()
amino_acids_with_stop = builtin_aaWithStop ()

foreign import bpcall "Alphabet:genetic_code_standard" builtin_standard_code :: () -> ()
standard_code = builtin_standard_code ()

