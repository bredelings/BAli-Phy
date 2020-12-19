module Data.CSV ( read_csv )
    where

import Foreign.String
import Foreign.Vector

map_vector_to_list f = map f . vector_to_list

builtin_read_csv :: CppString -> EVector (EVector CppString)
builtin builtin_read_csv 1 "read_csv" "Data"

-- This gives the csv file as a list of rows
read_csv :: String -> [ [ String ] ]
read_csv file = builtin_read_csv (pack_cpp_string file)

