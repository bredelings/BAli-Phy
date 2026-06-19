module Bio.Sequence where

import Data.Map as Map hiding (map)
import Bio.Alphabet
import Data.Text (Text)
import qualified Data.Text as T
import Data.BitVector
import Data.Maybe (isJust)

-- Dummy type that stands for the c++ `sequence` type
-- Can we eliminate this?
data ESequence = ESequence
foreign import bpcall "Alignment:sequence_name" builtin_sequence_name :: ESequence -> CPPString
foreign import bpcall "Alignment:" sequenceDataRaw :: ESequence -> CPPString

type Sequence = (Text,Text)

-- Map Text Text would be nicer, but discards the order of the sequences.
type Sequences = [Sequence]

mkSequence :: ESequence -> Sequence
mkSequence s = (T.fromCppString $ builtin_sequence_name s, T.fromCppString $ sequenceDataRaw s)

-- FIXME: make these operate on just the text, not the pair?
-- FIXME: remove sequence_to_indices in favor of stripGaps . sequenceToAlignedIndices?
foreign import bpcall "Alignment:sequence_to_indices" builtin_sequence_to_indices :: Alphabet a -> CPPString -> EVector Int
foreign import bpcall "Alignment:sequenceToAlignedIndices" builtin_sequenceToAlignedIndices :: Alphabet a -> CPPString -> EVector Int
sequence_to_indices a (_, s) = builtin_sequence_to_indices a (T.toCppString s)
sequenceToAlignedIndices a (_, s) = builtin_sequenceToAlignedIndices a (T.toCppString s)

-- sequence_to_indices :: Sequence -> [Int]
-- maybe add this later

foreign import bpcall "Alignment:" statesToLetters :: EVector Int -> EVector Int -> EVector Int

foreign import bpcall "Alignment:loadSequences" builtin_loadSequences :: CPPString -> IO (EVector ESequence)
loadSequences :: String -> IO [Sequence]
loadSequences filename = fmap (fmap mkSequence . vectorToList) $ builtin_loadSequences (list_to_string filename)

foreign import bpcall "Alignment:getRange" builtin_getRange :: CPPString -> Int -> EVector Int
foreign import bpcall "Alignment:" selectRangeRaw :: EVector Int -> CPPString -> CPPString
selectRange :: String -> [Sequence] -> [Sequence]
selectRange range sequences = let maxLength = maximum [ T.length $ snd s | s <- sequences ]
                                  range' = builtin_getRange (list_to_string range) maxLength
                                  select (name, chars) = (name, (T.fromCppString $ selectRangeRaw range' (T.toCppString chars)))
                               in fmap select sequences

reorderSequences names sequences | length names /= length sequences  = error "Sequences.reorderSequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (fst sequence, sequence) | sequence <- sequences ]

getSequenceLengths sequenceData = Map.fromList [ (label, vector_size isequence) | (label, isequence) <- getSequences $ sequenceData]

foreign import bpcall "Likelihood:" bitmaskFromSequence :: EVector Int -> CBitVector
foreign import bpcall "Likelihood:" stripGaps :: EVector Int -> EVector Int
foreign import bpcall "Likelihood:" maskSequenceRaw :: CBitVector -> EVector Int -> EVector Int

bitmaskFromSequence' s = BitVector $ bitmaskFromSequence s
maskSequence (BitVector bv) sequence = maskSequenceRaw bv sequence

fastaSeq (label, seq) = T.concat [T.singleton '>', label, T.singleton '\n', seq, T.singleton '\n']

fastaSeqs sequences = T.concat [fastaSeq s | s <- sequences]

data CharacterData a = CharacterData (Alphabet a) [(Text, EVector Int)]
data AlignedCharacterData a = Aligned (CharacterData a)
data UnalignedCharacterData a = Unaligned (CharacterData a)

instance HasAlphabet (CharacterData a) where
    type AlphabetOf (CharacterData a) = a
    getAlphabet (CharacterData a _) = a

instance HasAlphabet (AlignedCharacterData a) where
    type AlphabetOf (AlignedCharacterData a) = a
    getAlphabet (Aligned d) = getAlphabet d

instance HasAlphabet (UnalignedCharacterData a) where
    type AlphabetOf (UnalignedCharacterData a) = a
    getAlphabet (Unaligned d) = getAlphabet d

class HasSequences d where
    -- If we change the sequences to observations, then does this generalization still work?
    getSequences :: d -> [(Text, EVector Int)]

instance HasSequences (CharacterData a) where
    getSequences (CharacterData _ d) = d

instance HasSequences (AlignedCharacterData a) where
    getSequences (Aligned d) = getSequences d

instance HasSequences (UnalignedCharacterData a) where
    getSequences (Unaligned d) = getSequences d

getTaxa d = map fst $ getSequences d


mkCharacterData :: Alphabet a -> Sequences -> CharacterData a
mkCharacterData alphabet sequences = CharacterData alphabet [(label, go sequence) | (label, sequence) <- sequences]
    where go s = builtin_sequenceToAlignedIndices alphabet (T.toCppString s)

mkUnalignedCharacterData alphabet sequences = Unaligned (CharacterData alphabet indices')
    where CharacterData _ indices = mkCharacterData alphabet sequences
          indices' = map (\(label,is) -> (label, stripGaps is)) indices

allSameAs x xs = and ((x==) <$> xs)

allSame []                       = error "allSame: nothing to compare!"
allSame (x:xs) | allSameAs x xs  = Just x
               | otherwise       = Nothing

checkSameLengths d@(CharacterData _ sequences) | isJust $ allSame lengths = d
                                               | otherwise                = error "Sequences have different lengths!"
    where lengths = [vector_size x | (_,x) <- sequences]

mkAlignedCharacterData alphabet sequences = Aligned $ checkSameLengths $ mkCharacterData alphabet sequences

unalign (Aligned (CharacterData a sequences)) = Unaligned (CharacterData a [(l, stripGaps s) | (l,s) <- sequences])
