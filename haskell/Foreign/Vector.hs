{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Foreign.CList
import Foreign.String -- for subtract_int' and increment_int

data EVector a

get_vector_index :: EVector a -> Int -> a
builtin get_vector_index 2 "get_vector_index" "Vector"

vector_size :: EVector a -> Int
builtin vector_size 1 "vector_size" "Vector"

clist_to_vector :: CList a -> EVector a
builtin clist_to_vector 1 "clist_to_vector" "Vector"

data EMatrix a
fromVectors :: EVector (EVector a) -> EMatrix a
builtin fromVectors 1 "fromVectors" "Vector"

list_from_vector_of_size :: EVector a -> Int -> [a]
list_from_vector_of_size vec size = go vec 0# where
    go vec i = case subtract_int' i size of
                 0# -> []
                 _ -> get_vector_index vec i : go vec (increment_int i)

list_from_vector :: EVector a -> [a]
list_from_vector vec = list_from_vector_of_size vec (vector_size vec)

vector_to_list = list_from_vector

list_to_vector :: [a] -> EVector a
list_to_vector x = clist_to_vector (list_to_CList x)

pack_cpp_string = list_to_string
