module Bio.Sequence where

-- Dummy type that stands for the c++ `sequence` type
data Sequence = Sequence

builtin_load_sequences :: CPPString -> EVector Sequence
builtin builtin_load_sequences 1 "load_sequences" "Alignment"
load_sequences :: String -> EVector Sequence
load_sequences filename = builtin_load_sequences (list_to_string filename)

builtin_select_range :: CPPString -> EVector Sequence -> EVector Sequence
builtin builtin_select_range 2 "select_range" "Alignment"
select_range :: String -> EVector Sequence -> EVector Sequence
select_range range sequences = builtin_select_range (list_to_string range) sequences


    
