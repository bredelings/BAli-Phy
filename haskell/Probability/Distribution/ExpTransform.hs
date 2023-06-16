module Probability.Distribution.ExpTransform where
import Probability.Random

import Probability.Distribution.Normal
import Probability.Distribution.Exponential
import Probability.Distribution.Gamma
import Probability.Distribution.Laplace
import Probability.Distribution.Cauchy

data ExpTransform d = ExpTransform d

instance (Dist d, Result d ~ Double) => Dist (ExpTransform d) where
    type Result (ExpTransform d) = Double
    dist_name (ExpTransform dist) = "Log" ++ dist_name dist

instance (IOSampleable d, Result d ~ Double) => IOSampleable (ExpTransform d) where
    sampleIO (ExpTransform dist) = exp <$> sampleIO dist

instance (HasPdf d, Result d ~ Double) => HasPdf (ExpTransform d) where
    pdf (ExpTransform dist) x = pdf dist x / doubleToLogDouble x

instance (Dist1D d, Result d ~ Double) => Dist1D (ExpTransform d) where
    cdf (ExpTransform dist) x = cdf dist (log x)
    lower_bound (ExpTransform dist) = fmap exp $ lower_bound dist
    upper_bound (ExpTransform dist) = fmap exp $ upper_bound dist

instance (ContDist1D d, Result d ~ Double) => ContDist1D (ExpTransform d) where
    quantile (ExpTransform dist) p = exp $ quantile dist p

instance (Sampleable d, Result d ~ Double) => Sampleable (ExpTransform d) where
    sample (ExpTransform dist) = exp <$> sample dist

log_normal mu sigma = ExpTransform $ normal mu sigma
log_exponential mu = ExpTransform $ exponential mu
log_gamma a b = ExpTransform $ gamma a b
log_laplace m s = ExpTransform $ laplace m s
log_cauchy m s = ExpTransform $ cauchy m s
