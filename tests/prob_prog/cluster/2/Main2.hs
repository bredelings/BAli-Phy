module Main where

import System.Environment
import Probability

model = do

  n <- (1+) <$> geometric 0.33

  ys <- iid_set n (exponential 1)

  4 ~> normalDist (sum ys) 2

  return ["ys" %=% ys, "n" %=% length ys]

main = do

  mcmc $ model
