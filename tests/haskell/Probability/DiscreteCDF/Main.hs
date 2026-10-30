{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating (exp)
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFloat (isNaN)
import Data.Bool
import Data.Eq
import Data.Foldable (all)
import Data.Ord
import Numeric.Prob (toProb)
import Probability.Dist (Dist1D(cdf))
import Probability.Distribution.Binomial (binomial)
import Probability.Distribution.Geometric (geometric)
import Probability.Distribution.NegativeBinomial (negativeBinomial)
import Probability.Distribution.Poisson (poisson)
import System.IO (print)

near x y = abs (x-y) < 1.0e-12
relative x y = abs (x/y-1) < 1.0e-12

-- Density/sampling tests do not check CDF step locations or loss of complementary probability
-- tails. These checks protect inclusive real thresholds while these CDFs have separate methods.
main = do
    let geometricCDF = cdf (geometric 0.25)
        poissonCDF = cdf (poisson 1)
        binomialCDF = cdf (binomial 4 0.25)
        negativeCDF = cdf (negativeBinomial 2 0.25)
        cdfs = [geometricCDF, poissonCDF, binomialCDF, negativeCDF]
    print (all (\f -> f (-0.5) == 0 && f (1/2) == f 0 && f 1.9 == f 1
                      && f 1 > f 0 && f (1/0) == 1 && isNaN (f (0/0))) cdfs)
    print (near (geometricCDF 1) 0.4375 && near (poissonCDF 1) (2 * exp (-1))
           && near (binomialCDF 1) (189/256) && near (negativeCDF 3) 0.3671875)
    print (cdf (geometric 1) 0 == 1 && cdf (geometric 0) 100 == 0
           && cdf (poisson 0) 0 == 1 && cdf (binomial 0 0.5) 0 == 1
           && cdf (binomial 4 0) 0 == 1 && cdf (binomial 4 1) 3.9 == 0
           && cdf (binomial 4 1) 4 == 1 && cdf (negativeBinomial 0 0) 0 == 1
           && cdf (negativeBinomial 2 1) 0 == 1 && cdf (negativeBinomial 2 0) 100 == 0)
    let tiny = toProb 1.0e-20
    print (relative (cdf (binomial 1 (1-tiny)) 0) 1.0e-20
           && relative (cdf (geometric tiny) 0) 1.0e-20
           && relative (cdf (negativeBinomial 1 tiny) 0) 1.0e-20
           && relative (cdf (poisson 100) 0) (exp (-100)))
    print (near (cdf (geometric 0.01) 100) (cdf (negativeBinomial 1 0.01) 100)
           && near (cdf (geometric 0.99) 0) (cdf (negativeBinomial 1 0.99) 0)
           && cdf (binomial 4 0.25) 1.0e100 == 1
           && isNaN (cdf (poisson (-1)) 0))
