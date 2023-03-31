module Probability.Distribution.Gamma where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:" gamma_density  :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:" gamma_cdf      :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:" gamma_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:sample_gamma" builtin_sample_gamma :: Double -> Double -> RealWorld -> Double
sample_gamma a b = makeIO $ builtin_sample_gamma a b

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
                                        return [gamma_density a b x]

instance Sampleable Gamma where
    sample dist@(Gamma a b) = RanDistribution2 dist gamma_effect

-- do we need a "shifted" gamma so that slice sampling doesn't crash?
-- do we need it so that changing the "shift" parameter doesn't affect variables sampled from it?
                              
gamma_bounds = between 0 1
gamma_effect x = add_move $ slice_sample_real_random_variable x gamma_bounds

gammaDist a b = Gamma a b

gamma a b = sample $ gammaDist a b
