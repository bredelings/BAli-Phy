{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating (Pow(ln))
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFloat (isNaN)
import Data.Bool
import Data.Eq
import Data.Function (($))
import Data.Maybe (Maybe(Nothing))
import Probability.Dist (Dist1D(cdf, lower_bound, upper_bound), HasPdf(pdf), IOSampleable(sampleIO))
import Probability.Distribution.Uniform (uniform_int, uniform_int_quantile)
import System.IO (IO, print)

-- Check the public boundary semantics that let conditional integer distributions survive
-- singleton and temporarily reversed bounds without turning those bounds into ordinary support.
main :: IO ()
main = do
  invalidSample <- sampleIO $ uniform_int 5 3
  singletonSample <- sampleIO $ uniform_int 4 4
  let invalid = uniform_int 5 3
      singleton = uniform_int 4 4
      outsideLogDensity = ln $ pdf (uniform_int 0 3) 5
  print
    ( invalidSample == 5
      && singletonSample == 4
      && isNaN (ln $ pdf invalid 5)
      && isNaN (cdf invalid 4)
      && isNaN (uniform_int_quantile 5 3 4)
      && lower_bound invalid == Nothing
      && upper_bound invalid == Nothing
      && ln (pdf singleton 4) == 0
      && outsideLogDensity == -(1 / 0)
    )
