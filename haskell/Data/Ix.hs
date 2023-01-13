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

instance (Ix a, Ix b) => Ix (a,b) where
    range     ((s1,s2),(e1,e2))         = [(x,y) | x <- range (s1,e1), y <- range (s2,e2)]
    index     ((s1,s2),(e1,e2)) (v1,v2) = index (s1,e1) v1 * rangeSize (s2,e2) + index (s2,e2) v2
    inRange   ((s1,s2),(e1,e2)) (v1,v2) = inRange (s1,e1) v1 && inRange (s2,e2) v2
    rangeSize ((s1,s2),(e1,e2))         = rangeSize (s1,e1) * rangeSize (s2,e2)
