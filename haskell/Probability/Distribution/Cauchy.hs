module Probability.Distribution.Cauchy where

import Probability.Random
import MCMC

---- cauchy

foreign import bpcall "Distribution:" cauchy_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:" cauchy_quantile :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:" sample_cauchy :: Double -> Double -> IO Double

cauchy_bounds = realLine
cauchy_effect x = add_move $ sliceSample x cauchy_bounds

data Cauchy = Cauchy Double Double

instance Dist Cauchy where
    type Result Cauchy = Double
    dist_name _ = "Cauchy"

instance IOSampleable Cauchy where
    sampleIO (Cauchy m s) = sample_cauchy m s

instance HasPdf Cauchy where
    pdf (Cauchy m s) x = cauchy_density m s x

instance Dist1D Cauchy where
    cdf (Cauchy m s) x = undefined

instance ContDist1D Cauchy where
    quantile (Cauchy m s) p = cauchy_quantile m s p

instance HasAnnotatedPdf Cauchy where
    annotated_densities dist@(Cauchy m s) x = do
       in_edge "m" m
       in_edge "s" s
       return ([pdf dist x],())

instance Sampleable Cauchy where
    sample dist@(Cauchy mu sigma) = RanDistribution2 dist cauchy_effect

cauchy m s = Cauchy m s

---- half_cauchy

-- Let's assume that the distribution is (i) continuous and (ii) symmetric around 0.

data Half d = (Result d ~ Double) => Half d

instance Dist d => Dist (Half d) where
    type Result (Half d) = Double
    dist_name dist = "Half" ++ dist_name dist

instance IOSampleable d => IOSampleable (Half d) where
    sampleIO (Half dist) = abs <$> sampleIO dist

-- Ignoring 0 assumes the distribution is continuous.
instance HasPdf d => HasPdf (Half d) where
    pdf (Half dist) x | x < 0     = 0
                      | x == 0    = pdf dist x
                      | otherwise = pdf dist x + pdf dist (-x)

instance Dist1D d => Dist1D (Half d) where
    cdf (Half dist) x  | x > 0     = cdf dist x - cdf dist (-x)
                       | otherwise = 0

-- This assumes that dist is symmetric around 0.
instance ContDist1D d => ContDist1D (Half d) where
    quantile (Half dist) p = quantile dist ((p+1)/2)

instance HasPdf d => HasAnnotatedPdf (Half d) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable d => Sampleable (Half d) where
    sample (Half dist) = abs <$> sample dist

half_cauchy s = Half $ cauchy 0 s

