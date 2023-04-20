module Probability.Distribution.Shifted where

import Probability.Random

data Shifted d = Shifted d (Result d)

instance (Dist d, Num (Result d)) => Dist (Shifted d) where
    type Result (Shifted d) = Result d
    dist_name (Shifted dist _) = "shifted " ++ dist_name dist

instance (IOSampleable d, Num (Result d)) => IOSampleable (Shifted d) where
    sampleIO (Shifted dist delta) = (+ delta) <$> sampleIO dist

instance (HasPdf d, Num (Result d)) => HasPdf (Shifted d) where
    pdf (Shifted dist delta) x = pdf dist (x - delta)

instance (Dist1D d, Result (Shifted d) ~ Double) => Dist1D (Shifted d) where
    cdf (Shifted dist delta) x = cdf dist (x - delta)

instance (ContDist1D d, Result (Shifted d) ~ Double) => ContDist1D (Shifted d) where
    quantile (Shifted dist delta) p = quantile dist p + delta

instance (Sampleable d, Num (Result d)) => Sampleable (Shifted d) where
    sample (Shifted dist delta) = (+ delta) <$> sample dist
