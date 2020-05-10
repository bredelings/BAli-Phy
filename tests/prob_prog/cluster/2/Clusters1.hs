module Main where

import Probability

import Data.ReadFile

xs = read_file_as_double "xs"

n_points = length xs

cluster_dist = do
  mean <- normal 0.0 10.0
  prec <- gamma 1.0 1.0
  let sigma = 1.0/prec
  return (mean,sigma)

main = do
  alpha <- random $ gamma 0.5 10.0

  params <- random $ dp n_points alpha cluster_dist

  let dists = [normal mean sigma | (mean,sigma) <- params]

  observe (independent dists) xs

  return ["alpha" %=% alpha
         ,"params" %=% params
         ]
