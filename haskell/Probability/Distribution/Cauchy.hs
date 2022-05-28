module Probability.Distribution.Cauchy where

import Probability.Random
import MCMC

---- cauchy

foreign import bpcall "Distribution:cauchy_density" cauchy_density 3
foreign import bpcall "Distribution:cauchy_quantile" cauchy_quantile 3
foreign import bpcall "Distribution:sample_cauchy" builtin_sample_cauchy 3

cauchy_bounds = realLine
cauchy_effect x = add_move $ slice_sample_real_random_variable x cauchy_bounds
sample_cauchy m s = RandomStructure cauchy_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_cauchy m s state)))

cauchy m s = Distribution "cauchy" (make_densities $ cauchy_density m s) (cauchy_quantile m s) (sample_cauchy m s) cauchy_bounds

---- half_cauchy

half_cauchy_density m s x | x < m     = doubleToLogDouble 0.0
                          | otherwise = 2.0*cauchy_density m s x

sample_half_cauchy m s = do
  x <- cauchy m s
  return $ abs(x-m) + m

half_cauchy_quantile m s p = cauchy_quantile m s ((p+1.0)/2.0)
half_cauchy_bounds m = above m

half_cauchy m s = Distribution "half_cauchy" (make_densities $ half_cauchy_density m s) (half_cauchy_quantile m s) (sample_half_cauchy m s) (half_cauchy_bounds m)
