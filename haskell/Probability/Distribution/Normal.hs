module Probability.Distribution.Normal where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" normal_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:" normal_cdf :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:" normal_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:" sample_normal :: Double -> Double -> IO Double

data Normal = Normal Double Double

instance Dist Normal where
    type Result Normal = Double
    dist_name _ = "Normal"

instance IOSampleable Normal where
    sampleIO (Normal mu sigma) = sample_normal mu sigma

instance HasPdf Normal where
    pdf (Normal mu sigma) x = normal_density mu sigma x

instance Dist1D Normal where
    cdf (Normal mu sigma) x = normal_cdf mu sigma x

instance ContDist1D Normal where
    quantile (Normal mu sigma) p = normal_quantile mu sigma p

instance MaybeMean Normal where
    maybeMean (Normal mu _) = Just mu

instance Mean Normal

instance MaybeVariance Normal where
    maybeStdDev   (Normal _ sigma) = Just $ sigma

instance Variance Normal

instance HasAnnotatedPdf Normal where
    annotated_densities dist@(Normal mu sigma) x = do
                                               in_edge "mu" mu
                                               in_edge "sigma" sigma
                                               return ([pdf dist x], ())


{-
  So, to sample something for MCMC we need
  - pure sampling routine.
  + MCMC-specific:
    - effect
    - (dist_name dist)
    - annotated_densities dist
OR
  - sampling routine made up of other sampling actions.
 -}
instance Sampleable Normal where
    sample dist@(Normal mu sigma) = RanDistribution2 dist normal_effect

normal_bounds = realLine
normal_effect x = add_move $ slice_sample_real_random_variable x normal_bounds

normal mu sigma = Normal mu sigma

