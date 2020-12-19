module Probability.Distribution.Cauchy where

import Probability.Random
import MCMC

---- cauchy

builtin cauchy_density 3 "cauchy_density" "Distribution"
builtin cauchy_quantile 3 "cauchy_quantile" "Distribution"
builtin builtin_sample_cauchy 3 "sample_cauchy" "Distribution"

cauchy_bounds = realLine
cauchy_effect x = x `seq` bnds `seq` add_move (\c -> slice_sample_real_random_variable x bnds c) where bnds = c_range $ cauchy_bounds
sample_cauchy m s = RandomStructure cauchy_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_cauchy m s state)))

cauchy m s = Distribution (make_densities $ cauchy_density m s) (cauchy_quantile m s) (sample_cauchy m s) cauchy_bounds

---- half_cauchy

half_cauchy_density m s x | x < m     = doubleToLogDouble 0.0
                          | otherwise = 2.0*cauchy_density m s x

sample_half_cauchy m s = do
  x <- cauchy m s
  return $ abs(x-m) + m

half_cauchy_quantile m s p = cauchy_quantile m s ((p+1.0)/2.0)
half_cauchy_bounds m = above m

half_cauchy m s = Distribution (make_densities $ half_cauchy_density m s) (half_cauchy_quantile m s) (sample_half_cauchy m s) (half_cauchy_bounds m)

--- truncated_cauchy m s t = draw from cauchy until we are within > t

-- truncated t dist@(Distribution densities cdf quantile sample bounds) = Distribution densities' cdf' quantile' sample' bounds' where
--     cdf_t = cdf t
--     cdf' x | x < t     = return 0.0
--            | otherwise = (cdf x - cdf_t)/(1.0 - cdf_t)
--
--     quantile' p = quantile p' where p' = cdf_t + (1.0-cdf_t)*p
--     sample' = do
--       u <- uniform 0.0 1.0
--       return $ quantile' u
--     dens x = density dist x
--     density' x = density x / (1.0-cdf_t)
--     densities' x = [ density' x]
