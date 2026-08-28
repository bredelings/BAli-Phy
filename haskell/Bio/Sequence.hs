module Bio.Sequence where

import qualified Data.Map as Map
import Bio.Alphabet
import Compiler.FFI.Import (CInput)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bit (Bit)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (intVectorFromNativeWithLength)
import Foreign.NativeVector (NativeVector)
import Foreign.Vector (EVector, vector_size, vectorToList)
import Foreign.Pair (EPair, c_fst, c_snd, c_pair)
import Foreign.String (CPPString)

-- Opaque handle to the C++ `sequence` used while loading sequence files.
data ESequence
instance CInput ESequence

foreign import trcall "Alignment:sequence_name" sequenceName :: ESequence -> Text
foreign import trcall "Alignment:" sequenceDataRaw :: ESequence -> Text

type Sequence = (Text,Text)

-- Map Text Text would be nicer, but discards the order of the sequences.
type Sequences = [Sequence]

mkSequence :: ESequence -> Sequence
mkSequence s = (sequenceName s, sequenceDataRaw s)

foreign import trcall "Alignment:statesToLetters" statesToLetters :: U.Vector Int -> U.Vector Int -> U.Vector Int

foreign import trcall "Alignment:loadSequences" loadSequencesRaw :: String -> IO (EVector ESequence)
loadSequences :: String -> IO [Sequence]
loadSequences filename = fmap (fmap mkSequence . vectorToList) $ loadSequencesRaw filename

foreign import trcall "Alignment:getRange" getRange :: String -> Int -> U.Vector Int
foreign import trcall "Alignment:" selectRangeRaw :: U.Vector Int -> Text -> Text
selectRange :: String -> [Sequence] -> [Sequence]
selectRange range sequences = let maxLength = maximum [ T.length $ snd s | s <- sequences ]
                                  range' = getRange range maxLength
                                  select (name, chars) = (name, selectRangeRaw range' chars)
                               in fmap select sequences

reorderSequences names sequences | length names /= length sequences  = error "Sequences.reorderSequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (fst sequence, sequence) | sequence <- sequences ]

getSequenceLengths sequenceData = Map.fromList [(label, U.length sequence) | (label, sequence) <- getSequences sequenceData]

foreign import trcall "Likelihood:" bitmaskFromSequence :: U.Vector Int -> U.Vector Bit
foreign import trcall "Likelihood:" stripGaps :: U.Vector Int -> U.Vector Int
foreign import trcall "Likelihood:" maskSequence :: U.Vector Bit -> U.Vector Int -> U.Vector Int

fastaSeq (label, seq) = T.concat [T.singleton '>', label, T.singleton '\n', seq, T.singleton '\n']

fastaSeqs sequences = T.concat [fastaSeq s | s <- sequences]

data CharacterData = CharacterData Alphabet AmbiguityDatabase [(Text, U.Vector Int)]
newtype AlignedCharacterData = Aligned CharacterData
newtype UnalignedCharacterData = Unaligned CharacterData

instance HasAlphabet CharacterData where
    getAlphabet (CharacterData a _ _) = a

instance HasAlphabet AlignedCharacterData where
    getAlphabet (Aligned d) = getAlphabet d

instance HasAlphabet UnalignedCharacterData where
    getAlphabet (Unaligned d) = getAlphabet d

class HasSequences d where
    -- If we change the sequences to observations, then does this generalization still work?
    getSequences :: d -> [(Text, U.Vector Int)]

instance HasSequences CharacterData where
    getSequences (CharacterData _ _ d) = d

instance HasSequences AlignedCharacterData where
    getSequences (Aligned d) = getSequences d

instance HasSequences UnalignedCharacterData where
    getSequences (Unaligned d) = getSequences d

class HasAmbiguities d where
    getAmbiguities :: d -> AmbiguityDatabase

instance HasAmbiguities CharacterData where
    getAmbiguities (CharacterData _ ambiguities _) = ambiguities

instance HasAmbiguities AlignedCharacterData where
    getAmbiguities (Aligned d) = getAmbiguities d

instance HasAmbiguities UnalignedCharacterData where
    getAmbiguities (Unaligned d) = getAmbiguities d

getTaxa d = map fst $ getSequences d


foreign import bpcall "Alignment:emptyAmbiguityDatabase" emptyAmbiguityDatabase :: Alphabet -> AmbiguityDatabase

-- Construct character data whose codes are exact states or global special codes.
-- Database-local ambiguity codes must instead retain the database that encoded them.
mkExactCharacterData :: Alphabet -> [(Text, U.Vector Int)] -> CharacterData
mkExactCharacterData alphabet sequences = CharacterData alphabet (emptyAmbiguityDatabase alphabet) sequences

foreign import bpcall "Alignment:encodeCharacterData"
    encodeCharacterDataRaw :: Alphabet -> EVector (EPair CPPString CPPString) ->
                              EPair AmbiguityDatabase (EVector (EPair CPPString (EVector Int)))

-- Encode all input rows together so every negative ambiguity code refers to the
-- one database stored in the resulting CharacterData value.
mkCharacterData :: Alphabet -> Sequences -> CharacterData
mkCharacterData alphabet sequences = CharacterData alphabet ambiguities encoded
    where input = toVector [c_pair (T.toCppString label) (T.toCppString sequence) |
                            (label, sequence) <- sequences]
          encodedResult = encodeCharacterDataRaw alphabet input
          ambiguities = c_fst encodedResult
          encodedRaw = c_snd encodedResult
          encoded = [(T.fromCppString $ c_fst row, fromLegacySequenceVector $ c_snd row) |
                     row <- vectorToList encodedRaw]

mkUnalignedCharacterData alphabet sequences = Unaligned (CharacterData alphabet ambiguities indices')
    where CharacterData _ ambiguities indices = mkCharacterData alphabet sequences
          indices' = map (\(label,is) -> (label, stripGaps is)) indices

checkSameLengths (CharacterData _ _ []) = error "Cannot align an empty sequence collection!"
checkSameLengths d@(CharacterData _ _ ((_, first):rest))
    | all ((U.length first ==) . U.length . snd) rest = d
    | otherwise = error "Sequences have different lengths!"

mkAlignedCharacterData alphabet sequences = Aligned $ checkSameLengths $ mkCharacterData alphabet sequences

unalign (Aligned (CharacterData a ambiguities sequences)) =
    Unaligned (CharacterData a ambiguities [(l, stripGaps s) | (l,s) <- sequences])

-- TEMPORARY EVECTOR ADAPTER: copy directly between native Int storage and the boxed rows still
-- used by nested foreign interfaces. Delete these conversions when those interfaces accept
-- U.Vector Int.
foreign import trcall "NativeVector:intVectorToEVector"
    toLegacySequenceVector :: U.Vector Int -> EVector Int

foreign import bpcall "NativeVector:eVectorIntToNativeVector"
    eVectorIntToNativeVector :: EVector Int -> NativeVector Int

fromLegacySequenceVector :: EVector Int -> U.Vector Int
fromLegacySequenceVector vector =
    intVectorFromNativeWithLength (vector_size vector) (eVectorIntToNativeVector vector)
