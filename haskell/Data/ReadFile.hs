module Data.ReadFile where

import Foreign.String

builtin builtin_read_file_lines 1 "read_file_lines" "Data"
builtin cpp_string_to_double 1 "string_to_double" "Data"
builtin cpp_string_to_int 1 "string_to_int" "Data"

read_file_lines filename = list_from_vector $ builtin_read_file_lines (list_to_string filename)

read_file_as_double filename = map cpp_string_to_double $ read_file_lines filename

string_to_int    = cpp_string_to_int    . pack_cpp_string
string_to_double = cpp_string_to_double . pack_cpp_string
