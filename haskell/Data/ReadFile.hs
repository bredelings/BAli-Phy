module Data.ReadFile where

import Foreign.String

builtin "Data:read_file_lines" builtin_read_file_lines 1
builtin "Data:string_to_double" cpp_string_to_double 1
builtin "Data:string_to_int" cpp_string_to_int 1

read_file_lines filename = list_from_vector $ builtin_read_file_lines (list_to_string filename)

read_file_as_double filename = map cpp_string_to_double $ read_file_lines filename

-- NOTE: we already have read_int and read_double for reading from Haskell strings.
