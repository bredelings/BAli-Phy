module Main where

import Probability

import Data.ReadFile

xs = read_file_as_double "xs"

n_points = length xs

main = do

  means <- random $ independent $ repeat $ normal 0.0 10.0
  sigmas <- random $ independent $ repeat $ exponential 1.0
  let all_dists = [normal mean sigma | (mean,sigma) <- zip means sigmas]

  n_clusters' <- random $ geometric 0.33
  let n_clusters = 1+n_clusters'

  let dists = take n_clusters all_dists

  ps <- random $ symmetric_dirichlet n_clusters 0.5

  observe (iid n_points (mixture ps dists)) xs

  return ["n_clusters" %=% n_clusters,
          "weights" %=% ps,
          "mean" %=% take n_clusters means,
          "sigma" %=% take n_clusters sigmas]
