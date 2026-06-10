{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.Num
import Data.Eq
import System.IO (print)

data Color = Red | Blue | Green deriving Eq deriving Bounded
data Box a = Box a deriving Eq deriving Bounded

ok = minBound == Red
  && maxBound == Green
  && minBound == Box Red
  && maxBound == Box Green

main = print (if ok then (1 :: Int) else 0)
