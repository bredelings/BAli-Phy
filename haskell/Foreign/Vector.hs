{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Foreign.CList
import Data.Foldable
import Data.Eq
import Data.Function

data EVector a

foreign import ecall "Vector:" get_vector_index :: EVector a -> Int -> a

foreign import ecall "Vector:" vector_size :: EVector a -> Int

foreign import bpcall "Vector:" clist_to_vector :: CList a -> EVector a

sizedVectorToList :: EVector a -> Int -> [a]
sizedVectorToList vec size = mapFrom 0# size (\i -> get_vector_index vec i)

vectorToList :: EVector a -> [a]
vectorToList vec = sizedVectorToList vec (vector_size vec)

listToVector :: [a] -> EVector a
listToVector x = clist_to_vector (listToCList x)

instance Foldable EVector where
    toList = vectorToList
    length = vector_size
    null v = length v == 0

toVector = listToVector . toList
