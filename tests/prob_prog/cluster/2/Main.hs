module Main where

import Probability
import Data.Frame

cluster_dist = do
  mean <- cauchy 0.0 1.0
  sigma <- exponential 1.0
  return (mean, sigma)

prior = do

  n <- (1+) <$> geometric 0.33

  clusters <- iid n cluster_dist

  let dists = [normal mean sigma | (mean,sigma) <- clusters]

  ps <- symmetric_dirichlet n 0.5

  let loggers = ["n_clusters" %=% n,
                 "weights" %=% ps,
                 "clusters" %=% clusters]

  return (ps, dists, loggers)

observe_data xs = do

  (ps, dists, loggers) <- sample $ prior

  let n_points = length xs

  xs ~> iid n_points (mixture ps dists)

  return loggers

main = do
  xtable <- readTable "x.csv"
  let xs = xtable $$ ("x", AsDouble)

  let model = observe_data xs

  mcmc model
