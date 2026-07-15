{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector
    ( EVector
    , get_vector_index
    , vector_size
    , clist_to_vector
    , sizedVectorToList
    , vectorToList
    , listToVector
    , toVector
    ) where

import Compiler.FFI.Runtime (RuntimeValue)
import Compiler.FFI.Import (CInput, COutput)
import Foreign.CList
import Data.Foldable
import Data.Eq
import Data.Function

data EVector a

instance RuntimeValue (EVector a)

-- EVector is one opaque runtime object; translated boundaries do not traverse
-- or translate its elements.
instance CInput (EVector a)
instance COutput (EVector a)

foreign import ecall "Vector:" get_vector_index :: EVector a -> Int -> a

foreign import ecall "Vector:" vector_size :: EVector a -> Int

foreign import bpcall "Vector:clist_to_vector"
    clist_to_vector_raw :: CList a -> EVector a
foreign import bpcall "Vector:list_to_vector"
    list_to_vector_raw :: [a] -> EVector a

clist_to_vector :: CList a -> EVector a
clist_to_vector = clist_to_vector_raw

sizedVectorToList :: EVector a -> Int -> [a]
sizedVectorToList vec size = mapFrom 0# size (\i -> get_vector_index vec i)

vectorToList :: EVector a -> [a]
vectorToList vec = sizedVectorToList vec (vector_size vec)

listToVector :: RuntimeValue a => [a] -> EVector a
listToVector = list_to_vector_raw

instance Foldable EVector where
    toList = vectorToList
    length = vector_size
    null v = length v == 0

toVector :: (Foldable f, RuntimeValue a) => f a -> EVector a
toVector = listToVector . toList
