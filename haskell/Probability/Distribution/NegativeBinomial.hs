module Probability.Distribution.NegativeBinomial where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" negative_binomial_density :: Int -> Double -> Int -> LogDouble
foreign import bpcall "Distribution:" sample_negative_binomial :: Int -> Double -> IO Int

data NegativeBinomial = NegativeBinomial Int Prob

instance Dist NegativeBinomial where
    type Result NegativeBinomial = Int

instance IOSampleable NegativeBinomial where
    sampleIO (NegativeBinomial r p) = sample_negative_binomial r p

instance HasPdf NegativeBinomial where
    pdf (NegativeBinomial r p) = negative_binomial_density r p

instance Dist1D NegativeBinomial where
    cdf (NegativeBinomial r p) = undefined
    lower_bound _ = Just 0

instance MaybeMean NegativeBinomial where
    maybeMean (NegativeBinomial r p) = Just $ r * toFloating $ (1-p)/p

instance Mean NegativeBinomial where

instance MaybeVariance NegativeBinomial where
    maybeVariance (NegativeBinomial r p) = Just $ r * toFloating $ (1-p)/(p*p)

instance Variance NegativeBinomial

instance HasAnnotatedPdf NegativeBinomial where
    annotated_densities dist x = return [ pdf dist x ]

instance Sampleable NegativeBinomial where
    sample dist@(NegativeBinomial r p) = RanDistribution2 dist (negative_binomial_effect r)

negative_binomial_bounds = integer_above 0

negative_binomial_effect r x = do
    add_move $ slice_sample_integer_random_variable x negative_binomial_bounds
    add_move $ inc_dec_mh x negative_binomial_bounds

negativeBinomial :: Int -> Double -> NegativeBinomial
negativeBinomial r p = NegativeBinomial r (toFloating p)
