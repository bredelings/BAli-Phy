module Data.ReadFile where

import Foreign.String

foreign import bpcall "Data:read_file_lines" builtin_read_file_lines :: () -> ()
foreign import bpcall "Data:string_to_double" cpp_string_to_double :: () -> ()
foreign import bpcall "Data:string_to_int" cpp_string_to_int :: () -> ()

read_file_lines filename = list_from_vector $ builtin_read_file_lines (list_to_string filename)

read_file_as_double filename = map cpp_string_to_double $ read_file_lines filename

-- NOTE: we already have read_int and read_double for reading from Haskell strings.
