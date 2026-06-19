module Bio.Alphabet where

import Foreign.String -- For CPPString
import Data.Text (Text)
import qualified Data.Text as T

data DNA
data RNA
data AA
data AAWithStop
data Numeric
data Doublets a
data RNAEdits a
data Triplets a
data Codons a

data Alphabet a

data GeneticCode

class Nucleotide a
instance Nucleotide DNA
instance Nucleotide RNA

class AminoAcid a
instance AminoAcid AA
instance AminoAcid AAWithStop

class HasNucleotides a where
    type NucleotidesOf a
    getNucleotides :: Alphabet a -> Alphabet (NucleotidesOf a)

instance HasNucleotides (Doublets n) where
    type NucleotidesOf (Doublets n) = n
    getNucleotides = getNucleotidesRaw

instance HasNucleotides (RNAEdits n) where
    type NucleotidesOf (RNAEdits n) = n
    getNucleotides = getNucleotidesRaw

instance HasNucleotides (Triplets n) where
    type NucleotidesOf (Triplets n) = n
    getNucleotides = getNucleotidesRaw

instance HasNucleotides (Codons n) where
    type NucleotidesOf (Codons n) = n
    getNucleotides = getNucleotidesRaw

-- NOTE: Temporary bridge to the C++ runtime Alphabet representation.
-- Keep callers on HasNucleotides and remove this once C++ carries the type distinction.
foreign import bpcall "Alphabet:getNucleotides" getNucleotidesRaw :: Alphabet a -> Alphabet b

foreign import bpcall "Alphabet:getAminoAcids" getAminoAcids :: Alphabet (Codons n) -> Alphabet AA

foreign import ecall "Alphabet:" alphabetSize :: Alphabet a -> Int

foreign import bpcall "Alphabet:alphabet_letters" builtin_letters :: Alphabet a -> EVector CPPString
getLetters a = map listFromString (vectorToList (builtin_letters a) )

foreign import ecall "Alphabet:find_letter" builtin_find_letter :: Alphabet a -> CPPString -> Int
findLetter a letter = builtin_find_letter a (list_to_string letter)

foreign import ecall "Alphabet:" translate :: Alphabet (Codons n) -> Int -> Int

-- NOTE: Temporary bridge to C++ constructors that validate alphabet shape at runtime.
-- Keep callers on the constrained wrappers and remove this once C++ exposes typed entry points.
foreign import bpcall "Alphabet:mkDoublets" mkDoubletsRaw :: Alphabet n -> Alphabet (Doublets n)
mkDoublets :: Nucleotide n => Alphabet n -> Alphabet (Doublets n)
mkDoublets = mkDoubletsRaw

foreign import bpcall "Alphabet:mkRNAEdits" mkRNAEditsRaw :: Alphabet n -> Alphabet (RNAEdits n)
mkRNAEdits :: Nucleotide n => Alphabet n -> Alphabet (RNAEdits n)
mkRNAEdits = mkRNAEditsRaw

foreign import bpcall "Alphabet:mkTriplets" mkTripletsRaw :: Alphabet n -> Alphabet (Triplets n)
mkTriplets :: Nucleotide n => Alphabet n -> Alphabet (Triplets n)
mkTriplets = mkTripletsRaw

foreign import bpcall "Alphabet:mkCodons" mkCodonsRaw :: Alphabet n -> GeneticCode -> Alphabet (Codons n)
mkCodons :: Nucleotide n => Alphabet n -> GeneticCode -> Alphabet (Codons n)
mkCodons = mkCodonsRaw

foreign import bpcall "Alphabet:" dna :: Alphabet DNA

foreign import bpcall "Alphabet:" rna :: Alphabet RNA

foreign import bpcall "Alphabet:" aa :: Alphabet AA
amino_acids = aa

foreign import bpcall "Alphabet:aaWithStop" aaWithStop :: Alphabet AAWithStop
amino_acids_with_stop = aaWithStop

foreign import bpcall "Alphabet:" mkNumeric :: Int -> Alphabet Numeric

-- https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
foreign import bpcall "Alphabet:" geneticCodeByNumber :: Int -> GeneticCode
foreign import bpcall "Alphabet:" geneticCodeRaw :: CPPString -> GeneticCode
geneticCode name = geneticCodeRaw (pack_cpp_string name)

standard_code = geneticCodeByNumber 1

foreign import bpcall "Alphabet:" sequenceToTextRaw :: Alphabet a -> EVector Int -> CPPString
sequenceToText a s = T.fromCppString $ sequenceToTextRaw a s

class HasAlphabet x where
    type AlphabetOf x
    getAlphabet :: x -> Alphabet (AlphabetOf x)

gapCharIndex = -1 :: Int

missingCharIndex = -2 :: Int

type SMap = EVector Int

class HasAlphabet x => HasSMap x where
    getSMap :: x -> SMap

instance Show (Alphabet a) where
    show a = show (getLetters a)
