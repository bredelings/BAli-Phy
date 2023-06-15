module Bio.Sequence where

import Data.Map as Map
import Bio.Alphabet
import Data.Text (Text(..))

-- Dummy type that stands for the c++ `sequence` type
data Sequence = Sequence

foreign import bpcall "Alignment:sequence_name" builtin_sequence_name :: Sequence -> CPPString
sequence_name :: Sequence -> Text
sequence_name = Text . builtin_sequence_name

foreign import bpcall "Alignment:sequence_to_indices" sequence_to_indices :: Alphabet -> Sequence -> EVector Int
-- sequence_to_indices :: Sequence -> [Int]
-- maybe add this later

foreign import bpcall "Alignment:load_sequences" builtin_load_sequences :: CPPString -> IO (EVector Sequence)
load_sequences :: String -> IO [Sequence]
load_sequences filename = fmap list_from_vector $ builtin_load_sequences (list_to_string filename)

foreign import bpcall "Alignment:select_range" builtin_select_range :: CPPString -> EVector Sequence -> EVector Sequence
select_range :: String -> [Sequence] -> [Sequence]
select_range range sequences = list_from_vector $ builtin_select_range (list_to_string range) (list_to_vector sequences)

reorder_sequences names sequences | length names /= length sequences  = error "Sequences.reorder_sequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (sequence_name sequence, sequence) | sequence <- sequences ]

sequence_length a sequence = vector_size $ sequence_to_indices a sequence

get_sequence_lengths a sequences = Map.fromList [ (sequence_name sequence, sequence_length a sequence) | sequence <- sequences]
