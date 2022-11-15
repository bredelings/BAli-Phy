{-# LANGUAGE NoImplicitPrelude #-}
module Data.Unordered where

import Data.Functor
import Data.Foldable
import Compiler.Num

data Unordered a

foreign import bpcall "Array:" mkUnordered :: Int -> a -> Unordered a
foreign import bpcall "Array:" unorderedSize :: Unordered a -> Int
foreign import bpcall "Array:" unorderedGetIndex :: Unordered a -> Int -> a

foreign import bpcall "Array:" unorderedMap :: (a -> b) -> Unordered a -> Unordered b

-- foreign import bpcall "Array:" unorderedSum :: Unordered LogDouble -> LogDouble

instance Functor Unordered where
    fmap f u = unorderedMap f u

instance Foldable Unordered where
    toList u = [ unorderedGetIndex u i | i <- [0 .. unorderedSize u - 1]]
