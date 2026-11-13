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

-- Higher tuple indices use row-major order, with the last coordinate varying fastest.
-- Integer intermediates preserve the checked arithmetic of the pair instance.
instance (Ix a,Ix b,Ix c) => Ix (a,b,c) where
    range ((s1,s2,s3),(e1,e2,e3)) =
        [(v1,v2,v3) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3),(e1,e2,e3)) value@(v1,v2,v3) =
        checkedIndex (inRange bounds value) offset3
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)

    inRange ((s1,s2,s3),(e1,e2,e3)) (v1,v2,v3) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3),(e1,e2,e3)) =
        checkedRangeSize (inRange bounds (e1,e2,e3))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)))

instance (Ix a,Ix b,Ix c,Ix d) => Ix (a,b,c,d) where
    range ((s1,s2,s3,s4),(e1,e2,e3,e4)) =
        [(v1,v2,v3,v4) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4),(e1,e2,e3,e4)) value@(v1,v2,v3,v4) =
        checkedIndex (inRange bounds value) offset4
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)

    inRange ((s1,s2,s3,s4),(e1,e2,e3,e4)) (v1,v2,v3,v4) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4),(e1,e2,e3,e4)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e) => Ix (a,b,c,d,e) where
    range ((s1,s2,s3,s4,s5),(e1,e2,e3,e4,e5)) =
        [(v1,v2,v3,v4,v5) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5),(e1,e2,e3,e4,e5)) value@(v1,v2,v3,v4,v5) =
        checkedIndex (inRange bounds value) offset5
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)

    inRange ((s1,s2,s3,s4,s5),(e1,e2,e3,e4,e5)) (v1,v2,v3,v4,v5) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5),(e1,e2,e3,e4,e5)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f) => Ix (a,b,c,d,e,f) where
    range ((s1,s2,s3,s4,s5,s6),(e1,e2,e3,e4,e5,e6)) =
        [(v1,v2,v3,v4,v5,v6) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6),(e1,e2,e3,e4,e5,e6)) value@(v1,v2,v3,v4,v5,v6) =
        checkedIndex (inRange bounds value) offset6
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)

    inRange ((s1,s2,s3,s4,s5,s6),(e1,e2,e3,e4,e5,e6)) (v1,v2,v3,v4,v5,v6) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6),(e1,e2,e3,e4,e5,e6)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g) => Ix (a,b,c,d,e,f,g) where
    range ((s1,s2,s3,s4,s5,s6,s7),(e1,e2,e3,e4,e5,e6,e7)) =
        [(v1,v2,v3,v4,v5,v6,v7) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7),(e1,e2,e3,e4,e5,e6,e7)) value@(v1,v2,v3,v4,v5,v6,v7) =
        checkedIndex (inRange bounds value) offset7
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)

    inRange ((s1,s2,s3,s4,s5,s6,s7),(e1,e2,e3,e4,e5,e6,e7)) (v1,v2,v3,v4,v5,v6,v7) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7),(e1,e2,e3,e4,e5,e6,e7)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h) => Ix (a,b,c,d,e,f,g,h) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8),(e1,e2,e3,e4,e5,e6,e7,e8)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8),(e1,e2,e3,e4,e5,e6,e7,e8)) value@(v1,v2,v3,v4,v5,v6,v7,v8) =
        checkedIndex (inRange bounds value) offset8
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8),(e1,e2,e3,e4,e5,e6,e7,e8)) (v1,v2,v3,v4,v5,v6,v7,v8) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7,s8),(e1,e2,e3,e4,e5,e6,e7,e8)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i) => Ix (a,b,c,d,e,f,g,h,i) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9),(e1,e2,e3,e4,e5,e6,e7,e8,e9)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9),(e1,e2,e3,e4,e5,e6,e7,e8,e9)) value@(v1,v2,v3,v4,v5,v6,v7,v8,v9) =
        checkedIndex (inRange bounds value) offset9
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9),(e1,e2,e3,e4,e5,e6,e7,e8,e9)) (v1,v2,v3,v4,v5,v6,v7,v8,v9) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9),(e1,e2,e3,e4,e5,e6,e7,e8,e9)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i,Ix j) => Ix (a,b,c,d,e,f,g,h,i,j) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9),
            v10 <- range (s10,e10)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10))
          value@(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) =
        checkedIndex (inRange bounds value) offset10
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)
        offset10 = offset9 * intToInteger (rangeSize (s10,e10)) + intToInteger (index (s10,e10) v10)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)) (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9 &&
        inRange (s10,e10) v10

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)) *
             intToInteger (rangeSize (s10,e10)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i,Ix j,Ix k) => Ix (a,b,c,d,e,f,g,h,i,j,k) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9),
            v10 <- range (s10,e10),
            v11 <- range (s11,e11)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11))
          value@(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) =
        checkedIndex (inRange bounds value) offset11
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)
        offset10 = offset9 * intToInteger (rangeSize (s10,e10)) + intToInteger (index (s10,e10) v10)
        offset11 = offset10 * intToInteger (rangeSize (s11,e11)) + intToInteger (index (s11,e11) v11)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11))
            (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9 &&
        inRange (s10,e10) v10 &&
        inRange (s11,e11) v11

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)) *
             intToInteger (rangeSize (s10,e10)) *
             intToInteger (rangeSize (s11,e11)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i,Ix j,Ix k,Ix l) => Ix (a,b,c,d,e,f,g,h,i,j,k,l) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9),
            v10 <- range (s10,e10),
            v11 <- range (s11,e11),
            v12 <- range (s12,e12)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12))
          value@(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) =
        checkedIndex (inRange bounds value) offset12
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)
        offset10 = offset9 * intToInteger (rangeSize (s10,e10)) + intToInteger (index (s10,e10) v10)
        offset11 = offset10 * intToInteger (rangeSize (s11,e11)) + intToInteger (index (s11,e11) v11)
        offset12 = offset11 * intToInteger (rangeSize (s12,e12)) + intToInteger (index (s12,e12) v12)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12))
            (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9 &&
        inRange (s10,e10) v10 &&
        inRange (s11,e11) v11 &&
        inRange (s12,e12) v12

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)) *
             intToInteger (rangeSize (s10,e10)) *
             intToInteger (rangeSize (s11,e11)) *
             intToInteger (rangeSize (s12,e12)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i,Ix j,Ix k,Ix l,Ix m) => Ix (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9),
            v10 <- range (s10,e10),
            v11 <- range (s11,e11),
            v12 <- range (s12,e12),
            v13 <- range (s13,e13)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13))
          value@(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13) =
        checkedIndex (inRange bounds value) offset13
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)
        offset10 = offset9 * intToInteger (rangeSize (s10,e10)) + intToInteger (index (s10,e10) v10)
        offset11 = offset10 * intToInteger (rangeSize (s11,e11)) + intToInteger (index (s11,e11) v11)
        offset12 = offset11 * intToInteger (rangeSize (s12,e12)) + intToInteger (index (s12,e12) v12)
        offset13 = offset12 * intToInteger (rangeSize (s13,e13)) + intToInteger (index (s13,e13) v13)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13))
            (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9 &&
        inRange (s10,e10) v10 &&
        inRange (s11,e11) v11 &&
        inRange (s12,e12) v12 &&
        inRange (s13,e13) v13

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)) *
             intToInteger (rangeSize (s10,e10)) *
             intToInteger (rangeSize (s11,e11)) *
             intToInteger (rangeSize (s12,e12)) *
             intToInteger (rangeSize (s13,e13)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i,Ix j,Ix k,Ix l,Ix m,Ix n) => Ix
    (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9),
            v10 <- range (s10,e10),
            v11 <- range (s11,e11),
            v12 <- range (s12,e12),
            v13 <- range (s13,e13),
            v14 <- range (s14,e14)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14))
          value@(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) =
        checkedIndex (inRange bounds value) offset14
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)
        offset10 = offset9 * intToInteger (rangeSize (s10,e10)) + intToInteger (index (s10,e10) v10)
        offset11 = offset10 * intToInteger (rangeSize (s11,e11)) + intToInteger (index (s11,e11) v11)
        offset12 = offset11 * intToInteger (rangeSize (s12,e12)) + intToInteger (index (s12,e12) v12)
        offset13 = offset12 * intToInteger (rangeSize (s13,e13)) + intToInteger (index (s13,e13) v13)
        offset14 = offset13 * intToInteger (rangeSize (s14,e14)) + intToInteger (index (s14,e14) v14)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14))
            (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9 &&
        inRange (s10,e10) v10 &&
        inRange (s11,e11) v11 &&
        inRange (s12,e12) v12 &&
        inRange (s13,e13) v13 &&
        inRange (s14,e14) v14

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize
        bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)) *
             intToInteger (rangeSize (s10,e10)) *
             intToInteger (rangeSize (s11,e11)) *
             intToInteger (rangeSize (s12,e12)) *
             intToInteger (rangeSize (s13,e13)) *
             intToInteger (rangeSize (s14,e14)))

instance (Ix a,Ix b,Ix c,Ix d,Ix e,Ix f,Ix g,Ix h,Ix i,Ix j,Ix k,Ix l,Ix m,Ix n,Ix o) => Ix
    (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    range ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15)) =
        [(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) |
            v1 <- range (s1,e1),
            v2 <- range (s2,e2),
            v3 <- range (s3,e3),
            v4 <- range (s4,e4),
            v5 <- range (s5,e5),
            v6 <- range (s6,e6),
            v7 <- range (s7,e7),
            v8 <- range (s8,e8),
            v9 <- range (s9,e9),
            v10 <- range (s10,e10),
            v11 <- range (s11,e11),
            v12 <- range (s12,e12),
            v13 <- range (s13,e13),
            v14 <- range (s14,e14),
            v15 <- range (s15,e15)]

    -- Accumulate strides in Integer; check the final offset only after checking all coordinates.
    index bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15))
          value@(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) =
        checkedIndex (inRange bounds value) offset15
      where
        offset1 = intToInteger (index (s1,e1) v1)
        offset2 = offset1 * intToInteger (rangeSize (s2,e2)) + intToInteger (index (s2,e2) v2)
        offset3 = offset2 * intToInteger (rangeSize (s3,e3)) + intToInteger (index (s3,e3) v3)
        offset4 = offset3 * intToInteger (rangeSize (s4,e4)) + intToInteger (index (s4,e4) v4)
        offset5 = offset4 * intToInteger (rangeSize (s5,e5)) + intToInteger (index (s5,e5) v5)
        offset6 = offset5 * intToInteger (rangeSize (s6,e6)) + intToInteger (index (s6,e6) v6)
        offset7 = offset6 * intToInteger (rangeSize (s7,e7)) + intToInteger (index (s7,e7) v7)
        offset8 = offset7 * intToInteger (rangeSize (s8,e8)) + intToInteger (index (s8,e8) v8)
        offset9 = offset8 * intToInteger (rangeSize (s9,e9)) + intToInteger (index (s9,e9) v9)
        offset10 = offset9 * intToInteger (rangeSize (s10,e10)) + intToInteger (index (s10,e10) v10)
        offset11 = offset10 * intToInteger (rangeSize (s11,e11)) + intToInteger (index (s11,e11) v11)
        offset12 = offset11 * intToInteger (rangeSize (s12,e12)) + intToInteger (index (s12,e12) v12)
        offset13 = offset12 * intToInteger (rangeSize (s13,e13)) + intToInteger (index (s13,e13) v13)
        offset14 = offset13 * intToInteger (rangeSize (s14,e14)) + intToInteger (index (s14,e14) v14)
        offset15 = offset14 * intToInteger (rangeSize (s15,e15)) + intToInteger (index (s15,e15) v15)

    inRange ((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15))
            (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) =
        inRange (s1,e1) v1 &&
        inRange (s2,e2) v2 &&
        inRange (s3,e3) v3 &&
        inRange (s4,e4) v4 &&
        inRange (s5,e5) v5 &&
        inRange (s6,e6) v6 &&
        inRange (s7,e7) v7 &&
        inRange (s8,e8) v8 &&
        inRange (s9,e9) v9 &&
        inRange (s10,e10) v10 &&
        inRange (s11,e11) v11 &&
        inRange (s12,e12) v12 &&
        inRange (s13,e13) v13 &&
        inRange (s14,e14) v14 &&
        inRange (s15,e15) v15

    -- Check for an empty dimension before evaluating any potentially overflowing size.
    rangeSize
        bounds@((s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15),(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15)) =
        checkedRangeSize (inRange bounds (e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15))
            (intToInteger (rangeSize (s1,e1)) *
             intToInteger (rangeSize (s2,e2)) *
             intToInteger (rangeSize (s3,e3)) *
             intToInteger (rangeSize (s4,e4)) *
             intToInteger (rangeSize (s5,e5)) *
             intToInteger (rangeSize (s6,e6)) *
             intToInteger (rangeSize (s7,e7)) *
             intToInteger (rangeSize (s8,e8)) *
             intToInteger (rangeSize (s9,e9)) *
             intToInteger (rangeSize (s10,e10)) *
             intToInteger (rangeSize (s11,e11)) *
             intToInteger (rangeSize (s12,e12)) *
             intToInteger (rangeSize (s13,e13)) *
             intToInteger (rangeSize (s14,e14)) *
             intToInteger (rangeSize (s15,e15)))
