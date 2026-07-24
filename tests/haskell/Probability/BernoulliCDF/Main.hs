{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Probability.Dist (Dist1D(cdf))
import Probability.Distribution.Bernoulli (bernoulli)
import System.IO (print)

main = print [cdf distribution (-0.5), cdf distribution 0, cdf distribution 0.5, cdf distribution 1]
  where
    distribution = bernoulli 0.25
