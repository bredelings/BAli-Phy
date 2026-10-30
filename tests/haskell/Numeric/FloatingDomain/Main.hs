{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Numeric.Pow
import Compiler.Floating
import Compiler.Fractional
import Compiler.Classes
import Compiler.Num
import Compiler.RealFloat
import Data.Eq
import Data.Foldable (all)
import Data.Function ((.))
import Data.Floating.Types (FloatConvert(toFloating))
import Data.Ord
import Numeric.Log (Log)
import System.IO (print)
import Text.Show

infinity = (1.0 / 0.0 :: Double)
nan = (0.0 / 0.0 :: Double)

near x y = abs (x-y) < 1.0e-12

-- Density arithmetic tests do not check unordered comparisons. Keep both numeric types aligned
-- with GHC while their Ord instances expose NaNs through floating-point comparisons.
checkOrdering isNan n low middle high =
  ( all (\op -> all (\x -> not (op n x) && not (op x n)) [low, middle, high, n])
        [(<), (<=), (>), (>=)]
  , min n middle == middle && isNan (min middle n)
      && isNan (max n middle) && max middle n == middle
  , low < middle && low <= middle && middle > low && middle >= low
      && middle < high && high > middle && middle <= middle && middle >= middle
      && min low high == low && max low high == high
  )

-- Isolate floating-domain boundaries and Log Double primitives that full-program tests cannot diagnose.
-- Remove the log-number checks if Log Double no longer supplies the probability-number instances.
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
  let quarter = toFloating (0.25 :: Double) :: Log Double
      half = toFloating (0.5 :: Double) :: Log Double
      threeQuarters = toFloating (0.75 :: Double) :: Log Double
      huge = expTo 1000 :: Log Double
      nextHuge = expTo 999 :: Log Double
  print (near (toFloating quarter :: Double) 0.25, near (ln quarter) (log 0.25))
  print ( near (toFloating (quarter + threeQuarters) :: Double) 1.0
        , near (toFloating (threeQuarters - quarter) :: Double) 0.5
        , near (toFloating (quarter * half) :: Double) 0.125
        , near (toFloating (quarter / half) :: Double) 0.5
        )
  print ( near (ln (huge + nextHuge)) (1000 + log1p (exp (-1)))
        , near (ln (expTo 0 - expTo (-1) :: Log Double)) (log1mexp (-1))
        , near (ln (pow quarter 3)) (3 * log 0.25)
        )
  print (checkOrdering isNaN nan (-infinity) 1 infinity)
  print (checkOrdering (isNaN . ln) (expTo nan :: Log Double) 0 1 (expTo infinity))
  -- Equal signed zeros expose which argument max returns; GHC chooses the second.
  print (1 / max 0 (-0.0) == -infinity && 1 / max (-0.0) 0 == infinity)
