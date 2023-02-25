module Probability.Distribution.Binomial where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:binomial_density" binomial_density :: Int -> Double -> Int -> LogDouble
foreign import bpcall "Distribution:sample_binomial" builtin_sample_binomial :: Int -> Double -> RealWorld -> Int

binomial_bounds n = integer_between 0 n

binomial_effect n x = do
  add_move $ slice_sample_integer_random_variable x (binomial_bounds n)
  add_move $ inc_dec_mh x (binomial_bounds n)

sample_binomial n p = RanAtomic (binomial_effect n) (makeIO $ builtin_sample_binomial n p)

class HasBinomial d where
    binomial :: Int -> Double -> d Int

instance HasBinomial Distribution where
    binomial n p = Distribution "binomial" (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (binomial_bounds n)


instance HasBinomial Random where
    binomial n p = RanDistribution (binomial n p)
