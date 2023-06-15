module Probability.Distribution.Laplace where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" laplace_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:" sample_laplace  :: Double -> Double -> IO Double

data Laplace = Laplace Double Double

instance Dist Laplace where
    type Result Laplace = Double
    dist_name _ = "Laplace"

instance IOSampleable Laplace where
    sampleIO (Laplace m s) = sample_laplace m s

instance HasPdf Laplace where
    pdf (Laplace m s) x = laplace_density m s x

instance Dist1D Laplace where
    cdf (Laplace m s) x = undefined

instance ContDist1D Laplace where
    quantile (Laplace m s) p = undefined

instance MaybeMean Laplace where
    maybeMean (Laplace m s) = Just m

instance Mean Laplace

instance MaybeVariance Laplace where
    maybeVariance (Laplace _ s) = Just $ 2*s^2

instance Variance Laplace

instance HasAnnotatedPdf Laplace where
    annotated_densities dist@(Laplace m s) x = do
       in_edge "m" m
       in_edge "s" s
       return [pdf dist x]

instance Sampleable Laplace where
    sample dist@(Laplace mu sigma) = RanDistribution2 dist laplace_effect

laplace_bounds = realLine
laplace_effect x = add_move $ slice_sample_real_random_variable x laplace_bounds

laplace m s = Laplace m s
