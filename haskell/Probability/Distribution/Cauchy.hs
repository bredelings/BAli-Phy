module Probability.Distribution.Cauchy where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

---- cauchy

foreign import bpcall "Distribution:cauchy_density" cauchy_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:cauchy_quantile" cauchy_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:sample_cauchy" builtin_sample_cauchy :: Double -> Double -> RealWorld -> Double

cauchy_bounds = realLine
cauchy_effect x = add_move $ slice_sample_real_random_variable x cauchy_bounds
sample_cauchy m s = makeIO $ builtin_sample_cauchy m s
ran_sample_cauchy m s = RanAtomic cauchy_effect (sample_cauchy m s)

class HasCauchy d where
    cauchy :: Double -> Double -> d Double
    half_cauchy :: Double -> Double -> d Double

instance HasCauchy Distribution where
    cauchy m s = Distribution "cauchy" (make_densities $ cauchy_density m s) (cauchy_quantile m s) (ran_sample_cauchy m s) cauchy_bounds
    half_cauchy m s = Distribution "half_cauchy" (make_densities $ half_cauchy_density m s) (half_cauchy_quantile m s) (sample_half_cauchy m s) (half_cauchy_bounds m)

instance HasCauchy Random where
    cauchy m s = RanDistribution (cauchy m s)
    half_cauchy m s = RanDistribution $ half_cauchy m s

---- half_cauchy

half_cauchy_density m s x | x < m     = 0
                          | otherwise = 2 * cauchy_density m s x

sample_half_cauchy m s = do
  x <- cauchy m s
  return $ abs(x-m) + m

half_cauchy_quantile m s p = cauchy_quantile m s ((p+1)/2)
half_cauchy_bounds m = above m

