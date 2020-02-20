module Sequence where

builtin builtin_load_sequences 1 "load_sequences" "Alignment"
load_sequences filename = builtin_load_sequences (list_to_string filename)

builtin builtin_select_range 2 "select_range" "Alignment"
select_range range sequences = builtin_select_range (list_to_string range) sequences


    
