module Main where

import System.Environment
import Probability
import Data.Frame

cluster_dist = do
  mean <- cauchy 0.0 1.0
  sigma <- exponential 1.0
  return (mean, sigma)

model xs = do

  n <- (1+) <$> geometric 0.33

  clusters <- iid n cluster_dist

  ps <- symmetric_dirichlet n 0.5

  let n_points = length xs
      dists = [normal mean sigma | (mean,sigma) <- clusters]

  xs ~> iidDist n_points (mixture ps dists)

  return ["n_clusters" %=% n, "weights" %=% ps, "clusters" %=% clusters]

main = do
  [filename] <- getArgs

  xtable <- readTable filename

  let xs = xtable $$ "x" :: [Double]

  mcmc $ model xs
