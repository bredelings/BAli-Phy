module Probability.Distribution.Geometric where

import Probability.Random
import MCMC

--foreign import bpcall "Distribution:geometric_density" geometric_density :: Double -> Double -> Int -> LogDouble
foreign import bpcall "Distribution:" sample_geometric :: Double -> IO Int

data Geometric = Geometric Prob

instance Dist Geometric where
    type Result Geometric = Int
    dist_name _ = "geometric"

instance IOSampleable Geometric where
    sampleIO (Geometric p_success) = sample_geometric (toFloating p_success)


-- Hmm.. Maybe we want the result of discrete distributionts to be Probability instead of LogDouble?

instance HasPdf Geometric where
    pdf (Geometric p_success) n | n < 0 = 0
                                | otherwise = toFloating $ p_success * pow (1-p_success) (fromIntegral n)


instance Dist1D Geometric where
    cdf (Geometric p_success) n = undefined
    lower_bound dist = Just 0

instance MaybeMean Geometric where
    maybeMean (Geometric p) = Just $ toFloating $ (1-p)/p

instance Mean Geometric

instance MaybeVariance Geometric where
    maybeVariance (Geometric p) = Just $ toFloating $ (1-p)/(p*p)

instance Variance Geometric

instance HasAnnotatedPdf Geometric where
    annotated_densities dist@(Geometric p_success) n = do
                                     in_edge "p_success" p_success
                                     return ([pdf dist n],())

instance Sampleable Geometric where
    sample dist@(Geometric p_success) = RanDistribution2 dist geometric_effect

geometric_bounds = integer_above 0

geometric_effect x = do
  add_move $ sliceSampleInteger x geometric_bounds
  add_move $ incDecMH x geometric_bounds

{- NOTE: Why specify that pSuccess :: Double ?
   This avoids callers of (geometric p) getting
   saddled with ambiguous types.

   See note in Data.Floating.Types
-}

geometric :: Double -> Geometric
geometric pSuccess = Geometric (toFloating pSuccess)
