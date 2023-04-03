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

instance (ContDist1D d, Result d ~ Double) => ContDist1D (ExpTransform d) where
    quantile (ExpTransform dist) p = exp $ quantile dist p

instance (Sampleable d, Result d ~ Double) => Sampleable (ExpTransform d) where
    sample (ExpTransform dist) = exp <$> sample dist

logNormalDist mu sigma = ExpTransform $ normalDist mu sigma
logExponentialDist mu = ExpTransform $ exponentialDist mu
logGammaDist a b = ExpTransform $ gammaDist a b
logLaplace m s = ExpTransform $ laplaceDist m s
logCauchy m s = ExpTransform $ cauchyDist m s

log_normal mu sigma = sample $ logNormalDist mu sigma
log_exponential mu = sample $ logExponentialDist mu
log_gamma a b = sample $ logGammaDist a b
log_laplace m s = sample $ logLaplace m s
log_cauchy m s = sample $ logCauchy m s
