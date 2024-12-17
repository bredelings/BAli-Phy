module Data.CSV ( read_csv, read_tsv )
    where

import Foreign.String
import Foreign.Vector
import Data.Foldable

mapToList f = map f . toList

foreign import bpcall "Data:read_csv" builtin_read_csv :: CPPString -> Char -> IO (EVector (EVector CPPString))

-- This gives the csv file as a list of rows
read_csv :: String -> IO [ [ String ] ]
read_csv filename = mapToList (mapToList unpack_cpp_string) <$> builtin_read_csv filename' ','
    where filename' = pack_cpp_string filename

read_tsv :: String -> IO [ [ String ] ]
read_tsv filename = mapToList (mapToList unpack_cpp_string) <$> builtin_read_csv filename' '\t'
    where filename' = pack_cpp_string filename

