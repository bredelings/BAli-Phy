module Probability.Distribution.Cauchy where

import Probability.Random
import MCMC

---- cauchy

foreign import bpcall "Distribution:cauchy_density" cauchy_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:cauchy_quantile" cauchy_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:sample_cauchy" builtin_sample_cauchy :: Double -> Double -> RealWorld -> Double

cauchy_bounds = realLine
cauchy_effect x = add_move $ slice_sample_real_random_variable x cauchy_bounds
sample_cauchy m s = RandomStructure cauchy_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_cauchy m s state)))

class HasCauchy d where
    cauchy :: Double -> Double -> d Double

instance HasCauchy Distribution where
    cauchy m s = Distribution "cauchy" (make_densities $ cauchy_density m s) (cauchy_quantile m s) (sample_cauchy m s) cauchy_bounds

instance HasCauchy Random where
    cauchy m s = RanDistribution (cauchy m s)

---- half_cauchy

half_cauchy_density m s x | x < m     = 0
                          | otherwise = 2 * cauchy_density m s x

sample_half_cauchy m s = do
  x <- cauchy m s
  return $ abs(x-m) + m

half_cauchy_quantile m s p = cauchy_quantile m s ((p+1)/2)
half_cauchy_bounds m = above m

half_cauchy m s = Distribution "half_cauchy" (make_densities $ half_cauchy_density m s) (half_cauchy_quantile m s) (sample_half_cauchy m s) (half_cauchy_bounds m)
