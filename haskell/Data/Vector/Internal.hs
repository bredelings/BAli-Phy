{-# LANGUAGE NoImplicitPrelude #-}
module Data.Vector.Internal
    ( fromListNDefault
    , fromIndexedList
    ) where

import Compiler.Num
import Data.Vector (Vector)

foreign import bpcall "Vector:boxedFromListNDefault" fromListNDefault :: Int -> a -> [a] -> Vector a
foreign import bpcall "Vector:boxedFromIndexedList" fromIndexedList :: Int -> a -> [(Int,a)] -> Vector a
