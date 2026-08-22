module Bio.Sequence where

import qualified Data.Map as Map
import Bio.Alphabet
import Compiler.FFI.Import (CInput)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bit (Bit)
import qualified Data.Vector.Unboxed as U
import Foreign.Vector (EVector, listToVector, vectorToList)

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

foreign import trcall "Alignment:sequenceToAlignedIndices" sequenceToAlignedIndices :: Alphabet -> Text -> U.Vector Int

foreign import trcall "Alignment:statesToLetters" statesToLetters :: EVector Int -> U.Vector Int -> U.Vector Int

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

data CharacterData = CharacterData Alphabet [(Text, U.Vector Int)]
newtype AlignedCharacterData = Aligned CharacterData
newtype UnalignedCharacterData = Unaligned CharacterData

instance HasAlphabet CharacterData where
    getAlphabet (CharacterData a _) = a

instance HasAlphabet AlignedCharacterData where
    getAlphabet (Aligned d) = getAlphabet d

instance HasAlphabet UnalignedCharacterData where
    getAlphabet (Unaligned d) = getAlphabet d

class HasSequences d where
    -- If we change the sequences to observations, then does this generalization still work?
    getSequences :: d -> [(Text, U.Vector Int)]

instance HasSequences CharacterData where
    getSequences (CharacterData _ d) = d

instance HasSequences AlignedCharacterData where
    getSequences (Aligned d) = getSequences d

instance HasSequences UnalignedCharacterData where
    getSequences (Unaligned d) = getSequences d

getTaxa d = map fst $ getSequences d


mkCharacterData :: Alphabet -> Sequences -> CharacterData
mkCharacterData alphabet sequences = CharacterData alphabet [(label, go sequence) | (label, sequence) <- sequences]
    where go = sequenceToAlignedIndices alphabet

mkUnalignedCharacterData alphabet sequences = Unaligned (CharacterData alphabet indices')
    where CharacterData _ indices = mkCharacterData alphabet sequences
          indices' = map (\(label,is) -> (label, stripGaps is)) indices

checkSameLengths (CharacterData _ []) = error "Cannot align an empty sequence collection!"
checkSameLengths d@(CharacterData _ ((_, first):rest))
    | all ((U.length first ==) . U.length . snd) rest = d
    | otherwise = error "Sequences have different lengths!"

mkAlignedCharacterData alphabet sequences = Aligned $ checkSameLengths $ mkCharacterData alphabet sequences

unalign (Aligned (CharacterData a sequences)) = Unaligned (CharacterData a [(l, stripGaps s) | (l,s) <- sequences])

-- TEMPORARY EVECTOR ADAPTER: nested foreign interfaces still use boxed sequence rows.
-- Delete these conversions when those raw interfaces accept U.Vector Int.
toLegacySequenceVector :: U.Vector Int -> EVector Int
toLegacySequenceVector = listToVector . U.toList

fromLegacySequenceVector :: EVector Int -> U.Vector Int
fromLegacySequenceVector = U.fromList . vectorToList
