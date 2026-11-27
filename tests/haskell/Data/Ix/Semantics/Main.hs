{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import Data.Ix
import Data.Ord
import System.IO (print)

data Grid = Grid Int Int deriving Eq deriving Ord deriving Ix

tupleBounds :: ((Int,Int),(Int,Int))
tupleBounds = ((1,10),(2,11))

emptyAfterWideBounds :: ((Int,Int),(Int,Int))
emptyAfterWideBounds = ((-4611686018427387904,1),(4611686018427387903,0))

derivedEmptyAfterWideBounds :: (Grid,Grid)
derivedEmptyAfterWideBounds = (Grid (-4611686018427387904) 1, Grid 4611686018427387903 0)

maxIntSingleton :: (Int,Int)
maxIntSingleton = (9223372036854775807,9223372036854775807)

ok = rangeSize ((3 :: Int),1) == 0
  && rangeSize (((1,2),(2,1)) :: ((Int,Int),(Int,Int))) == 0
  && range tupleBounds == [(1,10),(1,11),(2,10),(2,11)]
  && index tupleBounds (2,10) == 2
  && range maxIntSingleton == [9223372036854775807]
  && rangeSize emptyAfterWideBounds == 0
  && rangeSize derivedEmptyAfterWideBounds == 0

main = print (if ok then (1 :: Int) else 0)
