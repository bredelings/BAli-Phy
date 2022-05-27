{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Foreign.CList

data EVector a

get_vector_index :: EVector a -> Int -> a
builtin get_vector_index 2 "Vector:get_vector_index"

vector_size :: EVector a -> Int
builtin vector_size 1 "Vector:vector_size"

clist_to_vector :: CList a -> EVector a
builtin clist_to_vector 1 "Vector:clist_to_vector"

data EMatrix a
fromVectors :: EVector (EVector a) -> EMatrix a
builtin fromVectors 1 "Vector:fromVectors"

list_from_vector_of_size :: EVector a -> Int -> [a]
list_from_vector_of_size vec size = map_from 0# size (\i -> get_vector_index vec i)

list_from_vector :: EVector a -> [a]
list_from_vector vec = list_from_vector_of_size vec (vector_size vec)

vector_to_list = list_from_vector

list_to_vector :: [a] -> EVector a
list_to_vector x = clist_to_vector (list_to_CList x)

