{-# LANGUAGE NoImplicitPrelude #-}
module Data.Vector.Internal
    ( fromListNDefault
    , fromIndexedList
    , replaceIndexed
    , accumIndexed
    ) where

import Compiler.Num
import Data.Vector (Vector)

foreign import bpcall "Vector:boxedFromListNDefault" fromListNDefault :: Int -> a -> [a] -> Vector a
foreign import bpcall "Vector:boxedFromIndexedList" fromIndexedList :: Int -> a -> [(Int,a)] -> Vector a
foreign import bpcall "Vector:boxedReplaceIndexed" replaceIndexed :: Vector a -> [(Int,a)] -> Vector a
foreign import bpcall "Vector:boxedAccumIndexed" accumIndexed :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
