module Probability.Distribution.Laplace where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:laplace_density" laplace_density :: () -> () -> () -> ()
foreign import bpcall "Distribution:sample_laplace" builtin_sample_laplace :: () -> () -> () -> ()

laplace_bounds = realLine
laplace_effect x = add_move $ slice_sample_real_random_variable x laplace_bounds
sample_laplace m s = RandomStructure laplace_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_laplace m s state)))

annotated_laplace_density m s x = do
  in_edge "m" m
  in_edge "s" s
  return [laplace_density m s x]

laplace m s = Distribution "laplace" (annotated_laplace_density m s) () (sample_laplace m s) laplace_bounds
