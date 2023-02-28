module Probability.Distribution.Laplace where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:laplace_density" laplace_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:sample_laplace" builtin_sample_laplace :: Double -> Double -> RealWorld -> Double

laplace_bounds = realLine
laplace_effect x = add_move $ slice_sample_real_random_variable x laplace_bounds
sample_laplace m s = makeIO $ builtin_sample_laplace m s
ran_sample_laplace m s = RanAtomic laplace_effect $ sample_laplace m s

annotated_laplace_density m s x = do
  in_edge "m" m
  in_edge "s" s
  return [laplace_density m s x]

data Laplace = Laplace Double Double


class HasLaplace d where
    laplace :: Double -> Double -> d Double

instance HasLaplace Distribution where
    laplace m s = Distribution "laplace" (annotated_laplace_density m s) (error "no quantile") (ran_sample_laplace m s) laplace_bounds

instance HasLaplace Random where
    laplace m s = RanDistribution $ laplace m s
