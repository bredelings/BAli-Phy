{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ix (Ix(..)) where

import Compiler.Enum
import Compiler.Num
import Data.Ord
import Data.Bool
import Data.Ix.Internal (checkedIndex, checkedRangeSize)

class Ord a => Ix a where
    range :: (a, a) -> [a]
    index :: (a, a) -> a -> Int
    inRange :: (a, a) -> a -> Bool
    rangeSize :: (a, a) -> Int

instance Ix Int where
    -- Stop at the upper bound before incrementing, including at maxInt.
    range (start,end)
        | start > end  = []
        | start == end = [start]
        | otherwise    = start : range (start + 1,end)

    index bounds@(start,_) val =
        checkedIndex (inRange bounds val)
                     (intToInteger val - intToInteger start)

    inRange (start,end) val = (start <= val) && (val <= end)

    rangeSize bounds@(start,end) =
        checkedRangeSize (inRange bounds end)
                         (intToInteger end - intToInteger start + 1)

instance (Ix a, Ix b) => Ix (a,b) where
    range ((s1,s2),(e1,e2)) =
        [(x,y) | x <- range (s1,e1), y <- range (s2,e2)]

    index bounds@((s1,s2),(e1,e2)) value@(v1,v2) =
        checkedIndex (inRange bounds value)
                     (intToInteger (index (s1,e1) v1) *
                      intToInteger (rangeSize (s2,e2)) +
                      intToInteger (index (s2,e2) v2))

    inRange ((s1,s2),(e1,e2)) (v1,v2) =
        inRange (s1,e1) v1 && inRange (s2,e2) v2

    rangeSize bounds@((s1,s2),(e1,e2)) =
        checkedRangeSize (inRange bounds (e1,e2))
                         (intToInteger (rangeSize (s1,e1)) *
                          intToInteger (rangeSize (s2,e2)))
