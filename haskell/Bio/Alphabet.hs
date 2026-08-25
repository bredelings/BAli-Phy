module Bio.Alphabet where

import Compiler.FFI.Import (CInput(..), COutput)
import Foreign.String -- For CPPString
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U

data Alphabet

instance CInput Alphabet

-- Opaque owner for ambiguity codes stored in character data.
data AmbiguityDatabase
instance CInput AmbiguityDatabase
instance COutput AmbiguityDatabase

data GeneticCode

-- This should only take a Triplets or Codons!
foreign import bpcall "Alphabet:" getNucleotides :: Alphabet -> Alphabet

-- This should only take a Triplets or Codons!
foreign import bpcall "Alphabet:" getAminoAcids :: Alphabet -> Alphabet

foreign import ecall "Alphabet:" alphabetSize :: Alphabet -> Int

foreign import bpcall "Alphabet:alphabet_letters" builtin_letters :: Alphabet -> EVector CPPString
getLetters a = map listFromString (vectorToList (builtin_letters a) )

-- Return the alphabet symbols as an unordered key domain while getLetters
-- continues to expose their underlying alphabet order.
letterSet a = Set.fromList (getLetters a)

foreign import ecall "Alphabet:find_letter" builtin_find_letter :: Alphabet -> CPPString -> Int
findLetter a letter = builtin_find_letter a (list_to_string letter)

foreign import ecall "Alphabet:" translate :: Alphabet -> Int -> Int

foreign import bpcall "Alphabet:" mkDoublets :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" mkRNAEdits :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" mkTriplets :: Alphabet -> Alphabet

foreign import bpcall "Alphabet:" mkCodons :: Alphabet -> GeneticCode -> Alphabet

foreign import bpcall "Alphabet:" dna :: Alphabet

foreign import bpcall "Alphabet:" rna :: Alphabet

foreign import bpcall "Alphabet:" aa :: Alphabet
amino_acids = aa

foreign import bpcall "Alphabet:aaWithStop" aaWithStop :: Alphabet
amino_acids_with_stop = aaWithStop

foreign import bpcall "Alphabet:" mkNumeric :: Int -> Alphabet

-- https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
foreign import bpcall "Alphabet:" geneticCodeByNumber :: Int -> GeneticCode
foreign import bpcall "Alphabet:" geneticCodeRaw :: CPPString -> GeneticCode
geneticCode name = geneticCodeRaw (pack_cpp_string name)

standard_code = geneticCodeByNumber 1

foreign import trcall "Alphabet:sequenceToTextRaw" sequenceToText :: Alphabet -> AmbiguityDatabase -> U.Vector Int -> Text

class HasAlphabet x where
    getAlphabet :: x -> Alphabet

gapCharIndex = -1 :: Int

missingCharIndex = -2 :: Int

type SMap = U.Vector Int

class HasAlphabet x => HasSMap x where
    getSMap :: x -> SMap

instance Show Alphabet where
    show a = show (getLetters a)
