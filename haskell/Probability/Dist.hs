{-# LANGUAGE DefaultSignatures #-}
module Probability.Dist (module Probability.Dist,
                         module Numeric.Log
                        ) where

import Numeric.Log
import Numeric.ProbDensity
import Data.Maybe (fromJust)

class Dist d where
    type Result d
    distName :: d -> String
    distName _ = "unnamed"

-- We can sample from these directly/atomically.
class Dist d => IOSampleable d where
    sampleIO :: d -> IO (Result d)

-- Define pdf for a scalar density with unit properties, or densityWithProperties for
-- separate factors and shared properties. At least one of these methods must be defined.
class Dist d => HasPdf d where
    type DistProperties d :: Type
    type DistProperties d = ()

    pdf :: d -> Result d -> Log Double
    pdf dist x = density dist x

    densityWithProperties :: d -> Result d -> ([ProbDensity], DistProperties d)
    default densityWithProperties :: DistProperties d ~ () =>
        d -> Result d -> ([ProbDensity], DistProperties d)
    densityWithProperties dist x = ([fromLogDensity $ pdf dist x], ())

-- Keep the factors intact for MCMC; collapse them only when a scalar density is requested.
densities :: HasPdf d => d -> Result d -> [ProbDensity]
densities dist x = fst $ densityWithProperties dist x

density :: HasPdf d => d -> Result d -> Log Double
density dist x = balancedProduct $ map collapseDensity $ densities dist x

pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balancedProduct xs = foldt (*) 1 xs

-- A one-dimensional distribution has an ordered result so its support bounds
-- can be compared as well as reported.
class (Dist d, Ord (Result d)) => Dist1D d where
    cdf :: d -> Double -> Double
    upper_bound :: d -> Maybe (Result d)
    lower_bound :: d -> Maybe (Result d)

    upper_bound _ = Nothing
    lower_bound _ = Nothing

bounds d = (lower_bound d, upper_bound d)

class (Dist d, Result d ~ Double) => ContDist1D d where
    quantile :: d -> Double -> Double

class Dist1D d => MaybeMean d where
    maybeMean :: d -> Maybe Double

class MaybeMean d => Mean d where
    mean :: d -> Double

    mean = fromJust . maybeMean

class MaybeMean d => MaybeVariance d where
    maybeVariance :: d -> Maybe Double
    maybeStdDev   :: d -> Maybe Double

    maybeVariance = fmap (\x -> x * x) . maybeStdDev
    maybeStdDev   = fmap sqrt . maybeVariance

class MaybeVariance d => Variance d where
   variance :: d -> Double
   stdDev   :: d -> Double

   variance = fromJust . maybeVariance
   stdDev   = fromJust . maybeStdDev
