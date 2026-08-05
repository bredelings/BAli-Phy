{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating (Pow(ln), log)
import Compiler.Num (Num(abs, (*), (+), (-)))
import Data.Bool ((&&))
import Data.Ord ((<))
import Probability.Dist (HasPdf(pdf))
import Probability.Distribution.Geometric (geometric, rgeometric)
import System.IO (IO, print)

-- Protect tail accuracy and reverse parameterization beyond direct Double subtraction.
-- This becomes redundant if geometric accepts a stable probability argument directly.
main :: IO ()
main =
    let tailActual = ln (pdf (geometric 0.6) 2000)
        tailExpected = log 0.6+2000*log 0.4
        reverseActual = ln (pdf (rgeometric 1.0e-20) 1)
        reverseExpected = log 1.0e-20
    in print (tailActual < 0 && abs (tailActual-tailExpected) < 1.0e-10
              && abs (reverseActual-reverseExpected) < 1.0e-10)
