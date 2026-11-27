{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.Num
import Data.Char (GeneralCategory(UppercaseLetter, NotAssigned))
import Data.Ord
import Data.Word (word8ToInt)
import System.IO (print)

data Color = Red | Blue | Green deriving Eq deriving Bounded
data Box a = Box a deriving Eq deriving Bounded

ok = minBound == Red
  && maxBound == Green
  && minBound == Box Red
  && maxBound == Box Green

-- Derived example types above do not exercise the Prelude instances, their constructor order,
-- or the primitive representations. Check their endpoints independently of the instance code.
standardBounds = intToInteger (minBound :: Int) == -9223372036854775808
  && intToInteger (maxBound :: Int) == 9223372036854775807
  && charToInt minBound == 0 && charToInt maxBound == 0x10ffff
  && word8ToInt minBound == 0 && word8ToInt maxBound == 255
  && compare False True == LT && compare True False == GT
  && minBound == (False, LT, ()) && maxBound == (True, GT, ())
  && (case (minBound, maxBound) of
        (UppercaseLetter, NotAssigned) -> True
        _ -> False)
  && maxBound == ((),(),(),(),(),(),(),(),(),(),(),(),(),(),True)

main = print (if ok && standardBounds then (1 :: Int) else 0)
