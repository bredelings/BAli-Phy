module Model where

import Probability
import System.Environment
import Data.Frame

-- Ideally, the categories and their weights would be exchangeable!
-- Currently if the first category is bad, there is not a good way
-- to eliminate it.

cluster_dist = do
  mean <- prior $ normal 0 10
  prec <- prior $ gamma 2 1
  let sigma = 1/prec
  return (mean,sigma)

model xs = do

  let n_points = length xs

  alpha <- prior $ gamma 0.5 10.0

  params <- prior $ dp n_points alpha cluster_dist

  observe xs $ independent [normal mean sigma | (mean,sigma) <- params]

  let loggers = ["alpha" %=% alpha, "params" %=% params]

  return loggers


main logDir = do
  [filename] <- getArgs

  xtable <- readTable filename

  let xs = xtable $$ "x" :: [Double]

  return $ model xs

