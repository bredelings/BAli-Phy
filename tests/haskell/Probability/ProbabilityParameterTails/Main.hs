{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating (log)
import Numeric.Pow (Pow(ln))
import Compiler.Num (Num(abs, (*), (+), (-)))
import Data.Bool ((&&))
import Data.Eq ((==))
import Data.Function (($))
import Data.Ord ((<))
import Numeric.Prob (toProb)
import Probability.Dist (HasPdf(pdf), IOSampleable(sampleIO))
import Probability.Distribution.Bernoulli (bernoulli)
import Probability.Distribution.Binomial (binomial)
import Probability.Distribution.Categorical (categorical)
import Probability.Distribution.Geometric (geometric)
import Probability.Distribution.Multinomial (multinomial)
import Probability.Distribution.NegativeBinomial (negativeBinomial)
import Probability.Distribution.Poisson (poisson)
import System.IO (IO, print)

-- Protect paired-tail densities and ensure zero or tiny success probabilities saturate beyond Int
-- range without a library retry loop. This is obsolete under an unbounded result type and another
-- exact probability representation.
main :: IO ()
main = do
    zeroGeometricSample <- sampleIO $ geometric 0.0
    saturatedGeometricSample <- sampleIO $ geometric 1.0e-300
    let p = 1 - toProb 1.0e-20
        expectedRare = log 1.0e-20
        bernoulliActual = ln (pdf (bernoulli p) 0)
        binomialActual = ln (pdf (binomial 2 p) 1)
        categoricalActual = ln (pdf (categorical [p, 1-p]) 1)
        geometricActual = ln (pdf (geometric p) 1)
        multinomialActual = ln (pdf (multinomial 2 [p, 1-p]) [1, 1])
        negativeBinomialActual = ln (pdf (negativeBinomial 2 p) 1)
        tailActual = ln (pdf (geometric 0.6) 2000)
        tailExpected = log 0.6+2000*log 0.4
    print (zeroGeometricSample == 9223372036854775807
              && saturatedGeometricSample == 9223372036854775807
              && tailActual < 0 && abs (tailActual-tailExpected) < 1.0e-10
              && abs (bernoulliActual-expectedRare) < 1.0e-10
              && abs (binomialActual-(log 2+expectedRare)) < 1.0e-10
              && abs (categoricalActual-expectedRare) < 1.0e-10
              && abs (geometricActual-expectedRare) < 1.0e-10
              && abs (multinomialActual-(log 2+expectedRare)) < 1.0e-10
              && abs (negativeBinomialActual-(log 2+expectedRare)) < 1.0e-10)
    -- At large counts the factorial terms can erase the whole log probability. Compare
    -- central and rare-event masses with independent values while counts fit in Int64.
    let n = 1000000000000000000
        rare = toProb 1.0e-18
        close x y = abs (x-y) < 1.0e-10
    print (close (ln (pdf (poisson 1.0e18) n)) (-21.642204370151084)
        && close (ln (pdf (binomial n rare) 1)) (-1)
        && close (ln (pdf (binomial n (1-rare)) (n-1))) (-1)
        && close (ln (pdf (negativeBinomial n (1-rare)) 1)) (-1)
        && close (ln (pdf (negativeBinomial 9223372036854775807 0.5) 9223372036854775807))
                 (-23.099648311122923))
