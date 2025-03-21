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
uniform_effect l u x = add_move $ sliceSample x (uniform_bounds l u)

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
uniform_int_effect l u x = do
  -- the slice sampling move refuses to add or remove variables.
  addMove (1/3) $ sliceSampleInteger x (uniform_int_bounds l u)
  addMove (1/3) $ discreteUniformAvoidMH x l u
  addMove (1/3) $ incDecMH x (uniform_int_bounds l u)

uniform_int l u = UniformInt l u

-------------------------------
data UniformD a = UniformD [a]

instance Dist (UniformD a) where
    type Result (UniformD a) = a
    dist_name _ = "uniformD"

instance IOSampleable (UniformD a) where
    sampleIO (UniformD values) = do
      index <- sampleIO $ uniform_int 0 (length values-1)
      return $ values !! index

instance Eq a => HasPdf (UniformD a) where
    pdf (UniformD values) x = sum [ if x == v then 1 else 0 | v <- values] / fromIntegral (length values)

instance Eq a => HasAnnotatedPdf (UniformD a) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable (UniformD a) where
    sample (UniformD values) = do
      index  <- sample $ uniform_int 0 (length values - 1)
      return $ values !! index

uniformD values = UniformD values
