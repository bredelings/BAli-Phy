module Probability.Distribution.ExpTransform where
import Probability.Random

import Probability.Distribution.Normal
import Probability.Distribution.Exponential
import Probability.Distribution.Gamma
import Probability.Distribution.Laplace
import Probability.Distribution.Cauchy
import Data.Floating.Types

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

------------------------------------------------------------------------------------------

data Shifted d = Shifted (Result d) d

instance Dist d => Dist (Shifted d) where
    type Result (Shifted d) = Result d
    dist_name (Shifted by dist) = "Shifted " ++ dist_name dist

instance (IOSampleable d, Num (Result d)) => IOSampleable (Shifted d) where
    sampleIO (Shifted by dist) = (by+) <$> sampleIO dist

instance (HasPdf d, Num (Result d)) => HasPdf (Shifted d) where
    pdf (Shifted by dist) x = pdf dist (x - by)

instance (Dist1D d, Result d ~ Double) => Dist1D (Shifted d) where
    cdf (Shifted by dist) x = cdf dist (x-by)
    lower_bound (Shifted by dist) = (by+) <$> lower_bound dist
    upper_bound (Shifted by dist) = (by+) <$> upper_bound dist

instance (ContDist1D d, Result d ~ Double) => ContDist1D (Shifted d) where
    quantile (Shifted by dist) p = (by+) $ quantile dist p

instance (Sampleable d, Num (Result d)) => Sampleable (Shifted d) where
    sample (Shifted by dist) = (by+) <$> sample dist

instance (MaybeMean d, Result d ~ Double) => MaybeMean (Shifted d) where
    maybeMean (Shifted by dist) = (by+) <$> maybeMean dist

instance (Mean d, Result d ~ Double) => Mean (Shifted d)

instance (MaybeVariance d, Result d ~ Double) => MaybeVariance (Shifted d) where
    maybeVariance (Shifted by dist) = maybeVariance dist

instance (Variance d, Result d ~ Double) => Variance (Shifted d)


shifted by dist = Shifted by dist

------------------------------------------------------------------------------------------

data Scaled d = Scaled (Result d) d

instance Dist d => Dist (Scaled d) where
    type Result (Scaled d) = Result d
    dist_name (Scaled by dist) = "Scaled " ++ dist_name dist

instance (IOSampleable d, Num (Result d)) => IOSampleable (Scaled d) where
    sampleIO (Scaled by dist) = (by*) <$> sampleIO dist

instance (HasPdf d, Fractional (Result d), FloatConvert (Result d) LogDouble) => HasPdf (Scaled d) where
    pdf (Scaled by dist) x = pdf dist (x/by) / (toFloating by)

instance (Dist1D d, Result d ~ Double) => Dist1D (Scaled d) where
    cdf (Scaled by dist) x = cdf dist (x/by)
    lower_bound (Scaled by dist) = (by*) <$> lower_bound dist
    upper_bound (Scaled by dist) = (by*) <$> upper_bound dist

instance (ContDist1D d, Result d ~ Double) => ContDist1D (Scaled d) where
    quantile (Scaled by dist) p = (by*) $ quantile dist p

instance (Sampleable d, Num (Result d)) => Sampleable (Scaled d) where
    sample (Scaled by dist) = (by*) <$> sample dist


instance (MaybeMean d, Result d ~ Double) => MaybeMean (Scaled d) where
    maybeMean (Scaled by dist) = (by*) <$> maybeMean dist

instance (Mean d, Result d ~ Double) => Mean (Scaled d)

instance (MaybeVariance d, Result d ~ Double) => MaybeVariance (Scaled d) where
    maybeVariance (Scaled by dist) = (by*by*) <$> maybeVariance dist

instance (Variance d, Result d ~ Double) => Variance (Scaled d)


scaled by dist = Scaled by dist

{- Perhaps it does make sense to allow distributions to have an unknown result.
   This would probably make things like scaling by a LogDouble work more simply.
-}
