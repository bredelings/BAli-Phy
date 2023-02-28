module Probability.Distribution.Gamma where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:shifted_gamma_density" shifted_gamma_density :: Double -> Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:shifted_gamma_quantile" shifted_gamma_quantile :: Double -> Double -> Double -> Double -> Double
foreign import bpcall "Distribution:sample_shifted_gamma" builtin_sample_shifted_gamma :: Double -> Double -> Double -> RealWorld -> Double

gamma_bounds shift = above shift
gamma_effect shift x = add_move $ slice_sample_real_random_variable x (gamma_bounds shift)
sample_shifted_gamma a b shift = makeIO $ builtin_sample_shifted_gamma a b shift
sample_gamma a b = sample_shifted_gamma a b 0
ran_sample_shifted_gamma a b shift = RanAtomic (gamma_effect shift) (sample_shifted_gamma a b shift)

annotated_shifted_gamma_density a b shift x = do
  in_edge "a" a
  in_edge "b" b
  in_edge "shift" shift
  return [shifted_gamma_density a b shift x]

---------------------

gamma_density a b = shifted_gamma_density a b 0.0
gamma_quantile a b = shifted_gamma_quantile a b 0.0
ran_sample_gamma a b = ran_sample_shifted_gamma a b 0.0

annotated_gamma_density a b x = do
  in_edge "a" a
  in_edge "b" b
  return [gamma_density a b x]


class HasGamma d where
    gamma :: Double -> Double -> d Double
    shifted_gamma :: Double -> Double -> Double -> d Double

instance HasGamma Distribution where
    gamma a b = Distribution "gamma" (annotated_gamma_density a b) (gamma_quantile a b) (ran_sample_gamma a b) (gamma_bounds 0.0)
    shifted_gamma a b shift = Distribution "shifted_gamma" (annotated_shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (ran_sample_shifted_gamma a b shift) (gamma_bounds shift)

instance HasGamma Random where
    gamma a b = RanDistribution (gamma a b)
    shifted_gamma a b shift = RanDistribution (shifted_gamma a b shift)
