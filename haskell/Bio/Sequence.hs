module Bio.Sequence where

import Data.Map as Map

-- Dummy type that stands for the c++ `sequence` type
data Sequence = Sequence

builtin_sequence_name :: Sequence -> CPPString
builtin builtin_sequence_name 1 "sequence_name" "Alignment"
sequence_name :: Sequence -> String
sequence_name = unpack_cpp_string . builtin_sequence_name

sequence_to_indices :: Sequence -> EVector Int
builtin sequence_to_indices 2 "sequence_to_indices" "Alignment"
-- sequence_to_indices :: Sequence -> [Int]
-- maybe add this later

builtin_load_sequences :: CPPString -> EVector Sequence
builtin builtin_load_sequences 1 "load_sequences" "Alignment"
load_sequences :: String -> [Sequence]
load_sequences filename = list_from_vector $ builtin_load_sequences $ list_to_string filename

builtin_select_range :: CPPString -> EVector Sequence -> EVector Sequence
builtin builtin_select_range 2 "select_range" "Alignment"
select_range :: String -> [Sequence] -> [Sequence]
select_range range sequences = list_from_vector $ builtin_select_range (list_to_string range) (list_to_vector sequences)

reorder_sequences names sequences | length names /= length sequences  = error "Sequences.reorder_sequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (sequence_name sequence, sequence) | sequence <- sequences ]

sequence_length a sequence = vector_size $ sequence_to_indices a sequence

get_sequence_lengths a sequences = Map.fromList [ (sequence_name sequence, sequence_length a sequence) | sequence <- sequences]
