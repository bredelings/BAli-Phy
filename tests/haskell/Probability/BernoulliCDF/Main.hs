{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Data.Ord ((<))
import Probability.Dist (Dist1D(cdf))
import Probability.Distribution.Bernoulli (bernoulli)
import System.IO (print)

main = print (f (-0.5), abs (f 0 - 0.75) < 1e-14, abs (f 0.5 - 0.75) < 1e-14, f 1)
  where
    f = cdf (bernoulli 0.25)
