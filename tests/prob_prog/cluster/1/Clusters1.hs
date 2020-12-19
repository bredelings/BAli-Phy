module Main where

import Probability
import Data.CSV

xs = map read_double $ head $ read_csv "xs"

n_points = length xs

cluster_dist = do
  mean <- normal 0.0 10.0
  prec <- gamma 1.0 1.0
  let sigma = 1.0/prec
  return (mean,sigma)

main = do
  alpha <- sample $ gamma 0.5 10.0

  params <- sample $ dp n_points alpha cluster_dist

  let dists = [normal mean sigma | (mean,sigma) <- params]

  xs ~> independent dists

  return ["alpha" %=% alpha
         ,"params" %=% params
         ]
