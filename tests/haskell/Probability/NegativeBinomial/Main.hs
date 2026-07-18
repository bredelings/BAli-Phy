{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Fractional
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Floating.Types (FloatConvert(toFloating))
import Data.Ord
import Probability.Dist (HasPdf(pdf), IOSampleable(sampleIO), Mean(mean), Variance(variance))
import Probability.Distribution.NegativeBinomial (negativeBinomial)
import System.IO (print)

near x y = abs (x - y) < 1.0e-12

-- Check that the density, moments, and deterministic sampler endpoints all use p as the success probability.
main = do
    let dist = negativeBinomial 2 0.25
        probability k = toFloating (pdf dist k) :: Double
        successCertain = negativeBinomial 2 1.0
        noSuccessesNeeded = negativeBinomial 0 0.25
    sample1 <- sampleIO successCertain
    sample0 <- sampleIO noSuccessesNeeded
    print (near (probability 1) 0.09375
        && near (probability 3) 0.10546875
        && pdf successCertain 0 == 1
        && pdf successCertain 1 == 0
        && pdf noSuccessesNeeded 0 == 1
        && pdf noSuccessesNeeded 1 == 0
        && near (mean dist) 6.0
        && near (variance dist) 24.0
        && sample1 == 0
        && sample0 == 0)
