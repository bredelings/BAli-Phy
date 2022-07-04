{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ix where

import Compiler.Enum
import Compiler.Num
import Data.Ord
import Data.Bool

class Ord a => Ix a where
    range :: (a, a) -> [a]
    index :: (a, a) -> a -> Int
    inRange :: (a, a) -> a -> Bool
    rangeSize :: (a, a) -> Int

instance Ix Int where
    range     (start,end)     = [start..end]
    index     (start,end) val = val - start
    inRange   (start,end) val = (start <= val) && (val <= end)
    rangeSize (start,end)     = end - start + 1
