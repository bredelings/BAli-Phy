module Bio.Sequence where

import Data.Map as Map hiding (map)
import Bio.Alphabet
import Data.Text (Text)
import qualified Data.Text as T
import Data.BitVector

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
-- FIXME: remove sequence_to_indices in favor of strip_gaps . sequenceToAlignedIndices?
foreign import bpcall "Alignment:sequence_to_indices" builtin_sequence_to_indices :: Alphabet -> CPPString -> EVector Int
foreign import bpcall "Alignment:sequenceToAlignedIndices" builtin_sequenceToAlignedIndices :: Alphabet -> CPPString -> EVector Int
sequence_to_indices a (_, s) = builtin_sequence_to_indices a (T.toCppString s)
sequenceToAlignedIndices a (_, s) = builtin_sequenceToAlignedIndices a (T.toCppString s)

-- sequence_to_indices :: Sequence -> [Int]
-- maybe add this later

foreign import bpcall "Alignment:" statesToLetters :: EVector Int -> EVector Int -> EVector Int

foreign import bpcall "Alignment:load_sequences" builtin_load_sequences :: CPPString -> IO (EVector ESequence)
load_sequences :: String -> IO [Sequence]
load_sequences filename = fmap (fmap mkSequence . list_from_vector) $ builtin_load_sequences (list_to_string filename)

foreign import bpcall "Alignment:getRange" builtin_getRange :: CPPString -> Int -> EVector Int
foreign import bpcall "Alignment:select_range" builtin_select_range :: EVector Int -> CPPString -> CPPString
select_range :: String -> [Sequence] -> [Sequence]
select_range range sequences = let maxLength = maximum [ T.length $ snd s | s <- sequences ]
                                   range' = builtin_getRange (list_to_string range) maxLength
                                   select (name, chars) = (name, (T.fromCppString $ builtin_select_range range' (T.toCppString chars)))
                               in fmap select sequences

reorder_sequences names sequences | length names /= length sequences  = error "Sequences.reorder_sequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (fst sequence, sequence) | sequence <- sequences ]

sequence_length a sequence = vector_size $ sequence_to_indices a sequence

get_sequence_lengths a sequences = Map.fromList [ (fst sequence, sequence_length a sequence) | sequence <- sequences]

foreign import bpcall "Likelihood:" bitmask_from_sequence :: EVector Int -> CBitVector
foreign import bpcall "Likelihood:" strip_gaps :: EVector Int -> EVector Int
foreign import bpcall "Likelihood:" maskSequenceRaw :: CBitVector -> EVector Int -> EVector Int

bitmask_from_sequence' s = BitVector $ bitmask_from_sequence s
maskSequence (BitVector bv) sequence = maskSequenceRaw bv sequence

fastaSeq (label, seq) = T.concat [T.singleton '>', label, T.singleton '\n', seq, T.singleton '\n']

fastaSeqs sequences = T.concat [fastaSeq s | s <- sequences]

data CharacterData = CharacterData Alphabet [(Text, EVector Int)]
data AlignedCharacterData = Aligned CharacterData
data UnalignedCharacterData = Unaligned CharacterData

instance HasAlphabet CharacterData where
    getAlphabet (CharacterData a _) = a

instance HasAlphabet AlignedCharacterData where
    getAlphabet (Aligned d) = getAlphabet d

instance HasAlphabet UnalignedCharacterData where
    getAlphabet (Unaligned d) = getAlphabet d

class HasSequences d where
    -- If we change the sequences to observations, then does this generalization still work?
    getSequences :: d -> [(Text, EVector Int)]

instance HasSequences CharacterData where
    getSequences (CharacterData _ d) = d

instance HasSequences AlignedCharacterData where
    getSequences (Aligned d) = getSequences d

instance HasSequences UnalignedCharacterData where
    getSequences (Unaligned d) = getSequences d

getTaxa d = map fst $ getSequences d


mkCharacterData :: Alphabet -> Sequences -> CharacterData
mkCharacterData alphabet sequences = CharacterData alphabet [(label, go sequence) | (label, sequence) <- sequences]
    where go s = builtin_sequenceToAlignedIndices alphabet (T.toCppString s)

mkUnalignedCharacterData alphabet sequences = Unaligned (CharacterData alphabet indices')
    where CharacterData _ indices = mkCharacterData alphabet sequences
          indices' = map (\(label,is) -> (label, strip_gaps is)) indices

-- We should check that the sequences are all the same length and error out otherwise.
mkAlignedCharacterData alphabet sequences = Aligned $ mkCharacterData alphabet sequences
