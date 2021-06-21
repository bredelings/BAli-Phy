module Probability.Distribution.Laplace where

import Probability.Random
import MCMC

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 3 "sample_laplace" "Distribution"

laplace_bounds = realLine
laplace_effect x = add_move $ slice_sample_real_random_variable x laplace_bounds
sample_laplace m s = RandomStructure laplace_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_laplace m s state)))

annotated_laplace_density m' s' x = do
  m <- in_edge "m" m'
  s <- in_edge "s" s'
  return [laplace_density m s x]

laplace m s = Distribution "laplace" (annotated_laplace_density m s) () (sample_laplace m s) laplace_bounds
