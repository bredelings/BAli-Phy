{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Foreign.CList

data EVector a

foreign import bpcall "Vector:" get_vector_index :: EVector a -> Int -> a

foreign import bpcall "Vector:" vector_size :: EVector a -> Int

foreign import bpcall "Vector:" clist_to_vector :: CList a -> EVector a

list_from_vector_of_size :: EVector a -> Int -> [a]
list_from_vector_of_size vec size = mapFrom 0# size (\i -> get_vector_index vec i)

list_from_vector :: EVector a -> [a]
list_from_vector vec = list_from_vector_of_size vec (vector_size vec)

vector_to_list = list_from_vector

list_to_vector :: [a] -> EVector a
list_to_vector x = clist_to_vector (list_to_CList x)

