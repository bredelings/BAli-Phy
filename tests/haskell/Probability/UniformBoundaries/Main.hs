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
import Data.OldList (all)
import Probability.Dist (ContDist1D(quantile), Mean(mean), Variance(variance))
import Probability.Dist (Dist1D(cdf, lower_bound, upper_bound), HasPdf(pdf), IOSampleable(sampleIO))
import Probability.Distribution.Uniform (uniform, uniform_int, uniform_int_quantile)
import System.IO (IO, print)

-- Check invalid conditional distributions through their public interface: ordinary runs rarely
-- exercise these boundaries. This group is needed while invalid bounds permit MCMC recovery.
main :: IO ()
main = do
  invalidSample <- sampleIO $ uniform_int 5 3
  singletonSample <- sampleIO $ uniform_int 4 4
  continuousPlaceholder <- sampleIO $ uniform 5 3
  nonfinitePlaceholder <- sampleIO $ uniform (0/0) (1/0)
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
      && continuousPlaceholder == 5
      && nonfinitePlaceholder == 0
      && pdf (uniform 0 2) 1 == 0.5
      && pdf (uniform 0 2) 3 == 0
      && isNaN (ln $ pdf (uniform 0 2) (0/0))
      && all invalidContinuous [uniform 5 3, uniform 4 4, uniform 0 (1/0), uniform (0/0) 1]
    )

-- All distribution queries must agree that these bounds do not define a continuous uniform.
invalidContinuous dist =
  isNaN (ln $ pdf dist 4) && isNaN (cdf dist 4) && isNaN (quantile dist 0.5)
  && isNaN (mean dist) && isNaN (variance dist)
  && lower_bound dist == Nothing && upper_bound dist == Nothing
