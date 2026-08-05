{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating (Pow(ln), log)
import Compiler.Num (Num(abs, (*), (+), (-)))
import Data.Bool ((&&))
import Data.Ord ((<))
import Numeric.Prob (toProb)
import Probability.Dist (HasPdf(pdf))
import Probability.Distribution.Bernoulli (bernoulli)
import Probability.Distribution.Binomial (binomial)
import Probability.Distribution.Categorical (categorical)
import Probability.Distribution.Geometric (geometric)
import Probability.Distribution.NegativeBinomial (negativeBinomial)
import System.IO (IO, print)

-- Protect log densities that depend on a complementary probability too small
-- for direct Double subtraction. This is obsolete under another exact probability representation.
main :: IO ()
main =
    let p = 1 - toProb 1.0e-20
        expectedRare = log 1.0e-20
        bernoulliActual = ln (pdf (bernoulli p) 0)
        binomialActual = ln (pdf (binomial 2 p) 1)
        categoricalActual = ln (pdf (categorical [p, 1-p]) 1)
        geometricActual = ln (pdf (geometric p) 1)
        negativeBinomialActual = ln (pdf (negativeBinomial 2 p) 1)
        tailActual = ln (pdf (geometric 0.6) 2000)
        tailExpected = log 0.6+2000*log 0.4
    in print (tailActual < 0 && abs (tailActual-tailExpected) < 1.0e-10
              && abs (bernoulliActual-expectedRare) < 1.0e-10
              && abs (binomialActual-(log 2+expectedRare)) < 1.0e-10
              && abs (categoricalActual-expectedRare) < 1.0e-10
              && abs (geometricActual-expectedRare) < 1.0e-10
              && abs (negativeBinomialActual-(log 2+expectedRare)) < 1.0e-10)
