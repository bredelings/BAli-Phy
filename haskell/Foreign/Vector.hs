{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Foreign.CList

data EVector a

get_vector_index :: EVector a -> Int -> a
foreign import bpcall "Vector:get_vector_index" get_vector_index 2

vector_size :: EVector a -> Int
foreign import bpcall "Vector:vector_size" vector_size 1

clist_to_vector :: CList a -> EVector a
foreign import bpcall "Vector:clist_to_vector" clist_to_vector 1

data EMatrix a
fromVectors :: EVector (EVector a) -> EMatrix a
foreign import bpcall "Vector:fromVectors" fromVectors 1

list_from_vector_of_size :: EVector a -> Int -> [a]
list_from_vector_of_size vec size = map_from 0# size (\i -> get_vector_index vec i)

list_from_vector :: EVector a -> [a]
list_from_vector vec = list_from_vector_of_size vec (vector_size vec)

vector_to_list = list_from_vector

list_to_vector :: [a] -> EVector a
list_to_vector x = clist_to_vector (list_to_CList x)

