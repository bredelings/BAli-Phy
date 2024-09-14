module Bio.Alphabet where

import Foreign.String -- For CPPString
import Data.Text (Text)
import qualified Data.Text as T

data Alphabet

data GeneticCode

-- This should only take a Triplets or Codons!
foreign import bpcall "Alphabet:" getNucleotides :: Alphabet -> Alphabet

-- This should only take a Triplets or Codons!
foreign import bpcall "Alphabet:" getAminoAcids :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" alphabetSize :: Alphabet -> Int

foreign import bpcall "Alphabet:alphabet_letters" builtin_letters :: Alphabet -> EVector CPPString
letters a = map listFromString (list_from_vector (builtin_letters a) )

foreign import bpcall "Alphabet:find_letter" builtin_find_letter :: Alphabet -> CPPString -> Int
find_letter a letter = builtin_find_letter a (list_to_string letter)

foreign import bpcall "Alphabet:" translate :: Alphabet -> Int -> Int

foreign import bpcall "Alphabet:" mkDoublets :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" mkRNAEdits :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" mkTriplets :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" mkCodons :: Alphabet -> GeneticCode -> Alphabet

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

foreign import bpcall "Alphabet:" mkNumeric :: Int -> Alphabet

-- https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
foreign import bpcall "Alphabet:" geneticCodeByNumber :: Int -> GeneticCode
foreign import bpcall "Alphabet:" geneticCodeRaw :: CPPString -> GeneticCode
geneticCode name = geneticCodeRaw (pack_cpp_string name)

standard_code = geneticCodeByNumber 1

foreign import bpcall "Alphabet:" sequenceToTextRaw :: Alphabet -> EVector Int -> CPPString
sequenceToText a s = T.fromCppString $ sequenceToTextRaw a s

class HasAlphabet x where
    getAlphabet :: x -> Alphabet

gapCharIndex = -1 :: Int

missingCharIndex = -2 :: Int

type SMap = EVector Int

class HasAlphabet x => HasSMap x where
    get_smap :: x -> SMap

instance Show Alphabet where
    show a = show (letters a)
