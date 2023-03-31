module Probability.Distribution.Beta where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:beta_density"  beta_density        :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:beta_cdf"      beta_cdf            :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:beta_quantile" beta_quantile       :: Double -> Double -> Double -> Double
foreign import bpcall "Distribution:sample_beta"   builtin_sample_beta :: Double -> Double -> RealWorld -> Double
sample_beta a b = makeIO $ builtin_sample_beta a b

data Beta = Beta Double Double

instance Dist Beta where
    type Result Beta = Double
    dist_name _ = "Beta"

instance IOSampleable Beta where
    sampleIO (Beta a b) = sample_beta a b

instance HasPdf Beta where
    pdf (Beta a b) x = beta_density a b x

instance Dist1D Beta where
    cdf (Beta a b) p = beta_cdf a b p

instance ContDist1D Beta where
    quantile (Beta a b) p = beta_quantile a b p

instance MaybeMean Beta where
    maybeMean (Beta a b) = Just (a * b)

instance Mean Beta

instance MaybeVariance Beta where
    maybeVariance (Beta a b) = Just $ a * b /((a+b)^2)/(a+b+a)

instance Variance Beta

instance HasAnnotatedPdf Beta where
    annotated_densities dist@(Beta a b) x = do
                                        in_edge "a" a
                                        in_edge "b" b
                                        return [beta_density a b x]

instance Sampleable Beta where
    sample dist@(Beta a b) = RanDistribution2 dist beta_effect

beta_bounds = between 0 1
beta_effect x = add_move $ slice_sample_real_random_variable x beta_bounds

betaDist a b = Beta a b

beta a b = sample $ betaDist a b
