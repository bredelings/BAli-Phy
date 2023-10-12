module Probability.Distribution.Uniform where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" uniform_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:" sample_uniform :: Double -> Double -> IO Double

data Uniform = Uniform Double Double

instance Dist Uniform where
    type Result Uniform = Double
    dist_name _ = "uniform"

instance IOSampleable Uniform where
    sampleIO (Uniform l u) = sample_uniform l u

instance HasPdf Uniform where
    pdf (Uniform l u) = uniform_density l u

instance Dist1D Uniform where
    cdf (Uniform l u) x | x < l     = 0
                        | x < u     = (x-l)/(u-l)
                        | otherwise = 1
    lower_bound (Uniform l r) = Just l
    upper_bound (Uniform l r) = Just r


instance ContDist1D Uniform where
    quantile (Uniform l u) p = l + p*(u-l)

instance MaybeMean Uniform where
    maybeMean (Uniform l u) = Just $ (l + u)/2

instance Mean Uniform

instance MaybeVariance Uniform where
    maybeVariance (Uniform l u) = Just $ (l-u)^2/12

instance Variance Uniform

instance HasAnnotatedPdf Uniform where
    annotated_densities dist@(Uniform l u) x = do
        in_edge "l" l
        in_edge "u" u
        return ([pdf dist x],())

instance Sampleable Uniform where
    sample dist@(Uniform l u) = RanDistribution2 dist (uniform_effect l u)


uniform l u = Uniform l u

uniform_bounds l u = between l u
uniform_effect l u x = add_move $ slice_sample_real_random_variable x (uniform_bounds l u)

------------------------------------
foreign import bpcall "Distribution:" uniform_int_density :: Int -> Int -> Int -> LogDouble
foreign import bpcall "Distribution:" sample_uniform_int :: Int -> Int -> IO Int

data UniformInt = UniformInt Int Int

instance Dist UniformInt where
    type Result UniformInt = Int
    dist_name _ = "uniform_int"

instance Dist1D UniformInt where
    cdf (UniformInt l u) x | floor x < l   = 0
                           | floor x < u   = fromIntegral (floor x-l+1)/ fromIntegral (u-l+1)
                           | otherwise     = 1

    lower_bound (UniformInt l u) = Just l
    upper_bound (UniformInt l u) = Just u

instance IOSampleable UniformInt where
    sampleIO (UniformInt l u) = sample_uniform_int l u

instance HasPdf UniformInt where
    pdf (UniformInt l u) = uniform_int_density l u

instance HasAnnotatedPdf UniformInt where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable UniformInt where
    sample dist@(UniformInt l u) = RanDistribution2 dist (uniform_int_effect l u)

uniform_int_quantile l u x | x <= l     = 0
                           | x > u      = 1
                           | otherwise  = fromIntegral (x-l) / fromIntegral (u-l+1)

uniform_int_bounds l u = integer_between l u
uniform_int_effect l u x = add_move $ slice_sample_integer_random_variable x (uniform_int_bounds l u)


uniform_int l u = UniformInt l u
