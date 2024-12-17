{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Foreign.CList

data EVector a

foreign import bpcall "Vector:" get_vector_index :: EVector a -> Int -> a

foreign import bpcall "Vector:" vector_size :: EVector a -> Int

foreign import bpcall "Vector:" clist_to_vector :: CList a -> EVector a

sizedVectorToList :: EVector a -> Int -> [a]
sizedVectorToList vec size = mapFrom 0# size (\i -> get_vector_index vec i)

vectorToList :: EVector a -> [a]
vectorToList vec = sizedVectorToList vec (vector_size vec)

list_to_vector :: [a] -> EVector a
list_to_vector x = clist_to_vector (listToCList x)

