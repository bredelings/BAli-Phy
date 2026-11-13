{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base (seq)
import Compiler.Num
import Compiler.Error (error)
import Data.Bool
import Data.Eq
import Data.Ord
import Data.Ix
import Data.Functor
import Data.Bifunctor
import Data.Semigroup
import Data.Monoid
import Control.DeepSeq
import Text.Show
import Text.Read
import System.IO (print)

-- Tuple instances are written separately for each arity, so check their field order
-- and boundary arities here. Scalar tests cannot detect omitted or transposed fields;
-- these checks can shrink if tuple instances are ever derived uniformly.
main = do
    let t5 = (1,2,3,4,5) :: (Int,Int,Int,Int,Int)
    print (read (show t5) == t5 && t5 < (1,2,3,4,6))
    let t6 = (1,2,3,4,5,6) :: (Int,Int,Int,Int,Int,Int)
    print (read (show t6) == t6 && t6 < (1,2,3,4,5,7))
    let t7 = (1,2,3,4,5,6,7) :: (Int,Int,Int,Int,Int,Int,Int)
    print (read (show t7) == t7 && t7 < (1,2,3,4,5,6,8))
    let t8 = (1,2,3,4,5,6,7,8) :: (Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t8) == t8 && t8 < (1,2,3,4,5,6,7,9))
    let t9 = (1,2,3,4,5,6,7,8,9) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t9) == t9 && t9 < (1,2,3,4,5,6,7,8,10))
    let t10 = (1,2,3,4,5,6,7,8,9,10) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t10) == t10 && t10 < (1,2,3,4,5,6,7,8,9,11))
    let t11 = (1,2,3,4,5,6,7,8,9,10,11) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t11) == t11 && t11 < (1,2,3,4,5,6,7,8,9,10,12))
    let t12 = (1,2,3,4,5,6,7,8,9,10,11,12) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t12) == t12 && t12 < (1,2,3,4,5,6,7,8,9,10,11,13))
    let t13 = (1,2,3,4,5,6,7,8,9,10,11,12,13) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t13) == t13 && t13 < (1,2,3,4,5,6,7,8,9,10,11,12,14))
    let t14 = (1,2,3,4,5,6,7,8,9,10,11,12,13,14) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t14) == t14 && t14 < (1,2,3,4,5,6,7,8,9,10,11,12,13,15))
    let t15 = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
    print (read (show t15) == t15 && t15 < (1,2,3,4,5,6,7,8,9,10,11,12,13,14,16))
    print t5
    let bounds3 = ((1,0,3),(2,0,4)) :: ((Int,Int,Int),(Int,Int,Int))
    print (range bounds3 == [(1,0,3), (1,0,4), (2,0,3), (2,0,4)]
        && [index bounds3 v | v <- range bounds3] == [0,1,2,3]
        && rangeSize bounds3 == 4
        && not (inRange bounds3 (3,0,3)))
    let bounds4 = ((1,0,0,3),(2,0,0,4)) :: ((Int,Int,Int,Int),(Int,Int,Int,Int))
    print (range bounds4 == [(1,0,0,3), (1,0,0,4), (2,0,0,3), (2,0,0,4)]
        && [index bounds4 v | v <- range bounds4] == [0,1,2,3]
        && rangeSize bounds4 == 4
        && not (inRange bounds4 (3,0,0,3)))
    let bounds5 = ((1,0,0,0,3),(2,0,0,0,4)) :: ((Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int))
    print (range bounds5 == [(1,0,0,0,3), (1,0,0,0,4), (2,0,0,0,3), (2,0,0,0,4)]
        && [index bounds5 v | v <- range bounds5] == [0,1,2,3]
        && rangeSize bounds5 == 4
        && not (inRange bounds5 (3,0,0,0,3)))
    let bounds6 = ((1,0,0,0,0,3),(2,0,0,0,0,4)) :: ((Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int))
    print (range bounds6 == [(1,0,0,0,0,3), (1,0,0,0,0,4), (2,0,0,0,0,3), (2,0,0,0,0,4)]
        && [index bounds6 v | v <- range bounds6] == [0,1,2,3]
        && rangeSize bounds6 == 4
        && not (inRange bounds6 (3,0,0,0,0,3)))
    let bounds7 = ((1,0,0,0,0,0,3),(2,0,0,0,0,0,4)) :: ((Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int))
    print (range bounds7 == [(1,0,0,0,0,0,3), (1,0,0,0,0,0,4), (2,0,0,0,0,0,3), (2,0,0,0,0,0,4)]
        && [index bounds7 v | v <- range bounds7] == [0,1,2,3]
        && rangeSize bounds7 == 4
        && not (inRange bounds7 (3,0,0,0,0,0,3)))
    let bounds8 =
            ((1,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds8 == [(1,0,0,0,0,0,0,3), (1,0,0,0,0,0,0,4), (2,0,0,0,0,0,0,3), (2,0,0,0,0,0,0,4)]
        && [index bounds8 v | v <- range bounds8] == [0,1,2,3]
        && rangeSize bounds8 == 4
        && not (inRange bounds8 (3,0,0,0,0,0,0,3)))
    let bounds9 =
            ((1,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds9 == [(1,0,0,0,0,0,0,0,3), (1,0,0,0,0,0,0,0,4), (2,0,0,0,0,0,0,0,3), (2,0,0,0,0,0,0,0,4)]
        && [index bounds9 v | v <- range bounds9] == [0,1,2,3]
        && rangeSize bounds9 == 4
        && not (inRange bounds9 (3,0,0,0,0,0,0,0,3)))
    let bounds10 =
            ((1,0,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds10 ==
        [(1,0,0,0,0,0,0,0,0,3),
         (1,0,0,0,0,0,0,0,0,4),
         (2,0,0,0,0,0,0,0,0,3),
         (2,0,0,0,0,0,0,0,0,4)]
        && [index bounds10 v | v <- range bounds10] == [0,1,2,3]
        && rangeSize bounds10 == 4
        && not (inRange bounds10 (3,0,0,0,0,0,0,0,0,3)))
    let bounds11 =
            ((1,0,0,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds11 ==
        [(1,0,0,0,0,0,0,0,0,0,3),
         (1,0,0,0,0,0,0,0,0,0,4),
         (2,0,0,0,0,0,0,0,0,0,3),
         (2,0,0,0,0,0,0,0,0,0,4)]
        && [index bounds11 v | v <- range bounds11] == [0,1,2,3]
        && rangeSize bounds11 == 4
        && not (inRange bounds11 (3,0,0,0,0,0,0,0,0,0,3)))
    let bounds12 =
            ((1,0,0,0,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds12 ==
        [(1,0,0,0,0,0,0,0,0,0,0,3),
         (1,0,0,0,0,0,0,0,0,0,0,4),
         (2,0,0,0,0,0,0,0,0,0,0,3),
         (2,0,0,0,0,0,0,0,0,0,0,4)]
        && [index bounds12 v | v <- range bounds12] == [0,1,2,3]
        && rangeSize bounds12 == 4
        && not (inRange bounds12 (3,0,0,0,0,0,0,0,0,0,0,3)))
    let bounds13 =
            ((1,0,0,0,0,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds13 ==
        [(1,0,0,0,0,0,0,0,0,0,0,0,3),
         (1,0,0,0,0,0,0,0,0,0,0,0,4),
         (2,0,0,0,0,0,0,0,0,0,0,0,3),
         (2,0,0,0,0,0,0,0,0,0,0,0,4)]
        && [index bounds13 v | v <- range bounds13] == [0,1,2,3]
        && rangeSize bounds13 == 4
        && not (inRange bounds13 (3,0,0,0,0,0,0,0,0,0,0,0,3)))
    let bounds14 =
            ((1,0,0,0,0,0,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds14 ==
        [(1,0,0,0,0,0,0,0,0,0,0,0,0,3),
         (1,0,0,0,0,0,0,0,0,0,0,0,0,4),
         (2,0,0,0,0,0,0,0,0,0,0,0,0,3),
         (2,0,0,0,0,0,0,0,0,0,0,0,0,4)]
        && [index bounds14 v | v <- range bounds14] == [0,1,2,3]
        && rangeSize bounds14 == 4
        && not (inRange bounds14 (3,0,0,0,0,0,0,0,0,0,0,0,0,3)))
    let bounds15 =
            ((1,0,0,0,0,0,0,0,0,0,0,0,0,0,3),(2,0,0,0,0,0,0,0,0,0,0,0,0,0,4))
            :: ((Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
    print (range bounds15 ==
        [(1,0,0,0,0,0,0,0,0,0,0,0,0,0,3),
         (1,0,0,0,0,0,0,0,0,0,0,0,0,0,4),
         (2,0,0,0,0,0,0,0,0,0,0,0,0,0,3),
         (2,0,0,0,0,0,0,0,0,0,0,0,0,0,4)]
        && [index bounds15 v | v <- range bounds15] == [0,1,2,3]
        && rangeSize bounds15 == 4
        && not (inRange bounds15 (3,0,0,0,0,0,0,0,0,0,0,0,0,0,3)))
    print (rangeSize (((-1073741824,0,1),(1073741823,0,0)) :: ((Int,Int,Int),(Int,Int,Int))) == 0)
    let m3 = ([0],[1],[2]) :: ([Int],[Int],[Int])
    print (mempty <> m3 == m3 && m3 <> ([9],[9],[9]) == ([0,9],[1,9],[2,9]))
    let m4 = ([0],[1],[2],[3]) :: ([Int],[Int],[Int],[Int])
    print (mempty <> m4 == m4 && m4 <> ([9],[9],[9],[9]) == ([0,9],[1,9],[2,9],[3,9]))
    let m5 = ([0],[1],[2],[3],[4]) :: ([Int],[Int],[Int],[Int],[Int])
    print (mempty <> m5 == m5 && m5 <> ([9],[9],[9],[9],[9]) == ([0,9],[1,9],[2,9],[3,9],[4,9]))
    print (fmap (\x -> x+1) ((0,1,2) :: (Int,Int,Int)) == (1,0,3))
    print (bimap (\x -> x+1) (\x -> x+2) ((0,1,2) :: (Int,Int,Int)) == (2,0,4))
    print (fmap (\x -> x+1) ((0,1,2,3) :: (Int,Int,Int,Int)) == (0,1,2,4))
    print (bimap (\x -> x+1) (\x -> x+2) ((0,1,2,3) :: (Int,Int,Int,Int)) == (0,1,3,5))
    print (fmap (\x -> x+1) ((0,1,2,3,4) :: (Int,Int,Int,Int,Int)) == (0,1,2,3,5))
    print (bimap (\x -> x+1) (\x -> x+2) ((0,1,2,3,4) :: (Int,Int,Int,Int,Int)) == (0,1,2,4,6))
    print (fmap (\x -> x+1) ((0,1,2,3,4,5) :: (Int,Int,Int,Int,Int,Int)) == (0,1,2,3,4,6))
    print (bimap (\x -> x+1) (\x -> x+2) ((0,1,2,3,4,5) :: (Int,Int,Int,Int,Int,Int)) == (0,1,2,3,5,7))
    print (fmap (\x -> x+1) ((0,1,2,3,4,5,6) :: (Int,Int,Int,Int,Int,Int,Int)) == (0,1,2,3,4,5,7))
    print (bimap (\x -> x+1) (\x -> x+2) ((0,1,2,3,4,5,6) :: (Int,Int,Int,Int,Int,Int,Int)) == (0,1,2,3,4,6,8))
    print (case fmap (\x -> x+1) (error "unused",2,3,4,5,6,7 :: Int) of
        (_,_,_,_,_,_,last) -> last == 8)
    print (rnf ((0,1,2,3,4,5) :: (Int,Int,Int,Int,Int,Int)) `seq` True)
    print (rnf ((0,1,2,3,4,5,6) :: (Int,Int,Int,Int,Int,Int,Int)) `seq` True)
    print (rnf ((0,1,2,3,4,5,6,7) :: (Int,Int,Int,Int,Int,Int,Int,Int)) `seq` True)
    print (rnf ((0,1,2,3,4,5,6,7,8) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int)) `seq` True)
