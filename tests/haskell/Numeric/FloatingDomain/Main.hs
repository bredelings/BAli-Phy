{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating
import Compiler.Fractional
import Compiler.Classes
import Compiler.Num
import Compiler.RealFloat
import Data.Eq
import Data.Ord
import System.IO (print)
import Text.Show

infinity = (1.0 / 0.0 :: Double)
nan = (0.0 / 0.0 :: Double)

-- Check that logarithms expose the platform's IEEE boundary and exceptional values.
main = do
  print (log (1.0 :: Double) == 0.0)
  print (isInfinite (log (0.0 :: Double)))
  print (log (0.0 :: Double) < 0.0)
  print (isNaN (log (-1.0 :: Double)))
  print (isInfinite (log infinity))
  print (isNaN (log nan))
  print (log1p (0.0 :: Double) == 0.0)
  print (isInfinite (log1p (-1.0 :: Double)))
  print (log1p (-1.0 :: Double) < 0.0)
  print (isNaN (log1p (-2.0 :: Double)))
  print (isInfinite (log1p infinity))
  print (isNaN (log1p nan))
