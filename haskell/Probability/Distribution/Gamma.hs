module Probability.Distribution.Gamma where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" gamma_density  :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:" gamma_cdf      :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:" gamma_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:" sample_gamma   :: Double -> Double -> IO Double

data Gamma = Gamma Double Double

instance Dist Gamma where
    type Result Gamma = Double
    dist_name _ = "Gamma"

instance IOSampleable Gamma where
    sampleIO (Gamma a b) = sample_gamma a b

instance HasPdf Gamma where
    pdf (Gamma a b) x = gamma_density a b x

instance Dist1D Gamma where
    cdf (Gamma a b) p = gamma_cdf a b p
    lower_bound _ = Just 0

instance ContDist1D Gamma where
    quantile (Gamma a b) p = gamma_quantile a b p

instance MaybeMean Gamma where
    maybeMean (Gamma a b) = Just (a * b)

instance Mean Gamma

instance MaybeVariance Gamma where
    maybeVariance (Gamma a b) = Just $ a * b * b

instance Variance Gamma

instance HasAnnotatedPdf Gamma where
    annotated_densities dist@(Gamma a b) x = do
                                        in_edge "a" a
                                        in_edge "b" b
                                        return ([gamma_density a b x], ())

instance Sampleable Gamma where
    sample dist@(Gamma a b) = RanDistribution2 dist gammaEffect

-- do we need a "shifted" gamma so that slice sampling doesn't crash?
-- do we need it so that changing the "shift" parameter doesn't affect variables sampled from it?
                              
gammaBounds = above 0
gammaEffect x = do
  addMove 1 $ sliceSample x gammaBounds

gamma a b = Gamma a b
