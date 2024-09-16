module Probability.Distribution.Binomial where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" binomial_density :: Int -> Double -> Int -> LogDouble
foreign import bpcall "Distribution:" sample_binomial :: Int -> Double -> IO Int

data Binomial = Binomial Int Prob

instance Dist Binomial where
    type Result Binomial = Int
    dist_name _ = "binomial"

instance IOSampleable Binomial where
    sampleIO (Binomial n p) = sample_binomial n (toFloating p)

instance HasPdf Binomial where
    pdf (Binomial n p) x = binomial_density n (toFloating p) x

instance Dist1D Binomial where
    cdf (Binomial n p) x  = undefined
    lower_bound (Binomial n p) = Just 0
    upper_bound (Binomial n p) = Just n

instance MaybeMean Binomial where
    maybeMean (Binomial n p) = Just $ fromIntegral n * (toFloating p)

instance Mean Binomial

instance MaybeVariance Binomial where
    maybeVariance (Binomial n p) = Just $ toFloating $ fromIntegral n * p * (1-p)

instance Variance Binomial

instance HasAnnotatedPdf Binomial where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable Binomial where
    sample dist@(Binomial n _) = RanDistribution2 dist (binomial_effect n)

binomial :: Int -> Double -> Binomial
binomial n p = Binomial n (toFloating p)

binomial_bounds n = integer_between 0 n

binomial_effect n x = do
  add_move $ sliceSampleInteger x (binomial_bounds n)
  add_move $ inc_dec_mh x (binomial_bounds n)
