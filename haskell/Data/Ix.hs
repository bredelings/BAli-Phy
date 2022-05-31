{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ix where

import Compiler.Enum
import Compiler.Num
import Data.Ord
import Data.Bool

range     (start,end) = [start..end]
index     (start,end) val = val - start
inRange   (start,end) val = (start <= val) && (val <= end)
rangeSize (start,end) = end - start + 1

class Ord a => Ix a
-- range :: (a, a) -> [a]
-- index :: (a, a) -> a -> Int
-- inRange :: (a, a) -> a -> Bool
-- rangeSize :: (a, a) -> Int
