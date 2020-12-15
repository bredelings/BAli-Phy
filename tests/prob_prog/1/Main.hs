module Main where

import Probability

import Data.ReadFile

xs = read_file_as_double "xs"

n_points = length xs

prior = do

  n_clusters <- (1+) `liftM` geometric 0.33

  means <- independent $ repeat $ normal 0.0 10.0
  sigmas <- independent $ repeat $ exponential 1.0
  let all_dists = [normal mean sigma | (mean,sigma) <- zip means sigmas]

  let dists = take n_clusters all_dists

  ps <- symmetric_dirichlet n_clusters 0.5

  let loggers = ["n_clusters" %=% n_clusters,
                 "weights" %=% ps,
                 "mean" %=% take n_clusters means,
                 "sigma" %=% take n_clusters sigmas]

  return (ps, dists, loggers)

main = do

  (ps, dists, loggers) <- random $ prior

  observe (iid n_points (mixture ps dists)) xs

  return loggers
