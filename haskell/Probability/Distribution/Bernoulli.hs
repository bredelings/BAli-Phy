module Probability.Distribution.Bernoulli where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:" sample_bernoulli :: Double -> IO Int

-- Could we allow this to return Int, Integer, Bool, etc?

-- Could we allow this to store the probability in an arbitrary Fractional or Pow type?

-- Should we put the result into the distribution class, a la (Dist Bernoulli Int)?

-- we could use fromRational... but converting to rationals seems like a terrible idea.

data Bernoulli = Bernoulli Prob

instance Dist Bernoulli where
    type Result Bernoulli = Int
    dist_name _ = "bernoulli"

instance IOSampleable Bernoulli where
    sampleIO (Bernoulli p) = sample_bernoulli $ toFloating $ p


instance HasPdf Bernoulli where
    pdf (Bernoulli p) n | n == 0    = toFloating $ 1-p
                        | n == 1    = toFloating $ p
                        | otherwise = 0

instance Dist1D Bernoulli where
    cdf (Bernoulli p) n | n < 0     = 0
                        | n == 1    = toFloating $ 1-p
                        | otherwise = 1
    lower_bound _ = Just 0
    upper_bound _ = Just 1

instance MaybeMean Bernoulli where
    maybeMean (Bernoulli p) = Just $ toFloating p

instance Mean Bernoulli

instance MaybeVariance Bernoulli where
    maybeVariance (Bernoulli p) = Just $ toFloating $ p * (1 - p)

instance Variance Bernoulli

instance HasAnnotatedPdf Bernoulli where
    annotated_densities dist@(Bernoulli p) n = do
       in_edge "p" p
       return ([pdf dist n],())

instance Sampleable Bernoulli where
    sample dist = RanDistribution2 dist bernoulli_effect

bernoulli_effect x = addMove 1 $ discreteUniformAvoidMH x 0 1

bernoulli :: Double -> Bernoulli
bernoulli p = Bernoulli (toFloating p)

rbernoulli q = Bernoulli (1-toFloating(q))


