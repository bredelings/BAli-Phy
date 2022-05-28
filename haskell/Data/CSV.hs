module Data.CSV ( read_csv, read_tsv )
    where

import Foreign.String
import Foreign.Vector

map_vector_to_list f = map f . vector_to_list

builtin_read_csv :: CPPString -> EVector (EVector CPPString)
foreign import bpcall "Data:read_csv" builtin_read_csv 2

-- This gives the csv file as a list of rows
read_csv :: String -> IO [ [ String ] ]
read_csv filename = IOAction (\s -> (s,map_vector_to_list (map_vector_to_list unpack_cpp_string) $ builtin_read_csv filename' ','))
    where filename' = pack_cpp_string filename

read_tsv :: String -> IO [ [ String ] ]
read_tsv filename = IOAction (\s -> (s,map_vector_to_list (map_vector_to_list unpack_cpp_string) $ builtin_read_csv filename' '\t'))
    where filename' = pack_cpp_string filename

