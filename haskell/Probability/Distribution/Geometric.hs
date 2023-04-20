module Probability.Distribution.Geometric where

import Probability.Random
import Control.Monad.IO.Class
import MCMC
import Range

--foreign import bpcall "Distribution:geometric_density" geometric_density :: Double -> Double -> Int -> LogDouble
foreign import bpcall "Distribution:sample_geometric" builtin_sample_geometric :: Double -> RealWorld -> Int
sample_geometric p_success = makeIO $ builtin_sample_geometric p_success

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

instance MaybeMean Geometric where
    maybeMean (Geometric p) = Just $ toFloating $ (1-p)/p

instance Mean Geometric

instance MaybeVariance Geometric where
    maybeVariance (Geometric p) = Just $ toFloating $ (1-p)/(p*p)

instance Variance Geometric

instance HasAnnotatedPdf Geometric where
    annotated_densities dist@(Geometric p_success) n = do
                                     in_edge "p_success" p_success
                                     return [pdf dist n]

instance Sampleable Geometric where
    sample dist@(Geometric p_success) = RanDistribution2 dist geometric_effect

geometric_bounds = integer_above 0

geometric_effect x = do
  add_move $ slice_sample_integer_random_variable x geometric_bounds
  add_move $ inc_dec_mh x geometric_bounds

geometric :: Double -> Geometric
geometric p_success = Geometric (toFloating p_success)

