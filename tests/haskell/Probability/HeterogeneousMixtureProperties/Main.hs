{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Probability.Dist (Dist1D(cdf, lower_bound, upper_bound), Mean(mean))
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.Mixture
import Probability.Distribution.Normal (normal)
import Probability.Distribution.Uniform (uniform)
import System.IO (print)

-- Check weighted CDF aggregation, delegated terminal bounds, and structural
-- bounds that include zero-weight components without normalizing zero tails.
main = print
    ( ( ( (cdf single 1, lower_bound single, upper_bound single)
        , (cdf bounded 1, lower_bound bounded, upper_bound bounded)
        )
      , ( cdf zeroSuffix 11
        , (lower_bound zeroWeightBounded, upper_bound zeroWeightBounded)
        , (lower_bound zeroWeightUnbounded, upper_bound zeroWeightUnbounded)
        )
      )
    , (mean meanSingle, mean meanMixture, mean meanZeroSuffix, mean meanScaled)
    )
  where
    single = 3 .*. uniform 0 2
    bounded = 1 .*. delta 0 |+| 2 .*. uniform 0 2 |+| 1 .*. delta 2
    zeroSuffix = 1 .*. delta 10 |+| 0 .*. normal 0 1 |+| 0 .*. delta 30
    zeroWeightBounded = 1 .*. delta 0 |+| 0 .*. uniform (-5) 10
    zeroWeightUnbounded = 1 .*. delta 0 |+| 0 .*. normal 0 1
    meanSingle = 5 .*. normal 2 3
    meanMixture = 1 .*. delta 0 |+| 2 .*. normal 3 2 |+| 1 .*. delta 8
    meanZeroSuffix = 1 .*. delta 10 |+| 0 .*. normal 0 1 |+| 0 .*. delta 30
    meanScaled = 7 .*. delta 0 |+| 14 .*. normal 3 2 |+| 7 .*. delta 8
