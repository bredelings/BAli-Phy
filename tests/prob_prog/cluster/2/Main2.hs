module Main where

import System.Environment
import Probability

model = do

  n <- (1+) <$> sample (geometric 0.33)

  ys <- iid n (exponential 1)

  observe 4 $ normal (sum ys) 2

  return ["ys" %=% ys, "n" %=% length ys]

main = do

  return $ model
