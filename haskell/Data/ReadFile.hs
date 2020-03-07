module Data.ReadFile where

builtin builtin_read_file_lines 1 "read_file_lines" "Data"
builtin string_to_double 1 "string_to_double" "Data"

read_file_lines filename = list_from_vector $ builtin_read_file_lines (list_to_string filename)

read_file_as_double filename = map string_to_double $ read_file_lines filename


