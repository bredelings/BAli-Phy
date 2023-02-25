module Probability.Distribution.Beta where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:beta_density" beta_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:beta_quantile" beta_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:sample_beta" builtin_sample_beta :: Double -> Double -> RealWorld -> Double

beta_bounds = between 0.0 1.0
beta_effect x = add_move $ slice_sample_real_random_variable x beta_bounds
sample_beta a b = RanAtomic beta_effect (makeIO $ builtin_sample_beta a b)

annotated_beta_density a b x = do
  in_edge "a" a
  in_edge "b" b
  return [beta_density a b x]

class HasBeta d where
    beta :: Double -> Double -> d Double

instance HasBeta Distribution where
    beta a b = Distribution "beta" (annotated_beta_density a b) (beta_quantile a b) (sample_beta a b) beta_bounds

instance HasBeta Random where
    beta a b = RanDistribution (beta a b)
