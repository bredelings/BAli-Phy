module Bio.Sequence where

-- Dummy type that stands for the c++ `sequence` type
data Sequence = Sequence

builtin_sequence_name :: Sequence -> CPPString
builtin builtin_sequence_name 1 "sequence_name" "Alignment"
sequence_name :: Sequence -> String
sequence_name = unpack_cpp_string . builtin_sequence_name

builtin_load_sequences :: CPPString -> EVector Sequence
builtin builtin_load_sequences 1 "load_sequences" "Alignment"
load_sequences :: String -> EVector Sequence
load_sequences filename = builtin_load_sequences (list_to_string filename)

builtin_select_range :: CPPString -> EVector Sequence -> EVector Sequence
builtin builtin_select_range 2 "select_range" "Alignment"
select_range :: String -> [Sequence] -> [Sequence]
select_range range sequences = list_from_vector $ builtin_select_range (list_to_string range) (list_to_vector sequences)


    
