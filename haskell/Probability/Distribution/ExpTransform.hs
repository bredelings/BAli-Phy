module Probability.Distribution.ExpTransform where
import Probability.Random

import Probability.Distribution.Normal
import Probability.Distribution.Exponential
import Probability.Distribution.Gamma
import Probability.Distribution.Laplace
import Probability.Distribution.Cauchy

-- This contains exp-transformed functions
expTransform dist@(Distribution name d q s r) = Distribution name' pdf' q' s' r'
 where
  pdf' x = do
    ds <- d x
    return $ (1 / doubleToLogDouble x):ds
  q'   = exp . q
  s'   = do v <- RanDistribution dist
            return $ exp v
  r'   = expTransformRange r
  name' = "log_"++name

class HasLogDists d where
    log_normal :: Double -> Double -> d Double
    log_exponential :: Double -> d Double
    log_gamma :: Double -> Double -> d Double
    log_laplace :: Double -> Double -> d Double
    log_cauchy :: Double -> Double -> d Double


instance HasLogDists Distribution where
    log_normal mu sigma = expTransform $ normal mu sigma
    log_exponential mu = expTransform $  exponential mu
    log_gamma a b = expTransform $ gamma a b
    log_laplace m s = expTransform $ laplace m s
    log_cauchy m s = expTransform $ cauchy m s

instance HasLogDists Random where
    log_normal mu sigma = RanDistribution $ log_normal mu sigma
    log_exponential mu = RanDistribution $ log_exponential mu
    log_gamma a b = RanDistribution $ log_gamma a b
    log_laplace m s = RanDistribution $ log_laplace m s
    log_cauchy m s = RanDistribution $ log_cauchy m s
