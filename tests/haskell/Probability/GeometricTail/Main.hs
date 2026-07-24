{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Floating (Pow(ln), log)
import Compiler.Num (Num(abs, (*), (+), (-)))
import Data.Bool ((&&))
import Data.Ord ((<))
import Probability.Dist (HasPdf(pdf))
import Probability.Distribution.Geometric (geometric)
import System.IO (IO, print)

-- Check that a far-tail geometric probability remains finite and accurate.
main :: IO ()
main =
    let actual = ln (pdf (geometric 0.6) 2000)
        expected = log 0.6+2000*log 0.4
    in print (actual < 0 && abs (actual-expected) < 1.0e-10)
