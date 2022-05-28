module Bio.Alphabet where

import Foreign.String -- For CPPString

data Alphabet

data GeneticCode

-- This should only take a Triplets or Codons!
foreign import bpcall "Alphabet:getNucleotides" getNucleotides :: Alphabet -> Alphabet

-- This should only take a Triplets or Codons!
foreign import bpcall "Alphabet:getAminoAcids" getAminoAcids :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:alphabetSize" alphabetSize :: Alphabet -> Int

foreign import bpcall "Alphabet:alphabet_letters" builtin_letters :: Alphabet -> EVector CPPString
letters a = map listFromString (list_from_vector (builtin_letters a) )

foreign import bpcall "Alphabet:find_letter" builtin_find_letter :: Alphabet -> CPPString -> Int
find_letter a letter = builtin_find_letter a (list_to_string letter)

foreign import bpcall "Alphabet:translate" translate :: Alphabet -> Int -> Int

foreign import bpcall "Alphabet:doublets" doublets :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:triplets" triplets :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:codons" codons :: Alphabet -> GeneticCode -> Alphabet

foreign import bpcall "Alphabet:dna" builtin_dna :: () -> Alphabet
dna = builtin_dna ()

foreign import bpcall "Alphabet:rna" builtin_rna :: () -> Alphabet
rna = builtin_rna ()

foreign import bpcall "Alphabet:aa" builtin_aa  :: () -> Alphabet
aa  = builtin_aa  ()
amino_acids = builtin_aa

foreign import bpcall "Alphabet:aaWithStop" builtin_aaWithStop :: () -> Alphabet
aaWithStop = builtin_aaWithStop ()
amino_acids_with_stop = builtin_aaWithStop ()

foreign import bpcall "Alphabet:genetic_code_standard" builtin_standard_code :: () -> GeneticCode
standard_code = builtin_standard_code ()

