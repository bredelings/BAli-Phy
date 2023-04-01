module Probability.Dist (module Probability.Dist,
                         module Numeric.LogDouble
                        ) where

import Numeric.LogDouble

class Dist d where
    type Result d
    dist_name :: d -> String
    dist_name _ = "unnamed"

-- We can sample from these directly/atomically.
class Dist d => IOSampleable d where
    sampleIO :: d -> IO (Result d)

-- We can compute the density/probability for these.
class Dist d => HasPdf d where
    pdf :: d -> Result d -> LogDouble

class Dist d => Dist1D d where
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
