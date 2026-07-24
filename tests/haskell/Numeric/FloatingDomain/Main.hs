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

near x y = abs (x-y) < 1.0e-12

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
  print (near (logsum 0.0 0.0) (log 2.0))
  print (logsum (-infinity) 2.0 == 2.0)
  print (isInfinite (logexpm1 0.0) && logexpm1 0.0 < 0.0)
  print (near (logexpm1 (log 2.0)) 0.0)
  print (near (logexpm1 1.0e-20) (log 1.0e-20))
  print (near (logexpm1 1000.0) 1000.0)
