module Probability.Distribution.ExpTransform where
import Probability.Random

import Probability.Distribution.Normal
import Probability.Distribution.Exponential
import Probability.Distribution.Gamma
import Probability.Distribution.Laplace
import Probability.Distribution.Cauchy

-- This contains exp-transformed functions
expTransform (Distribution d q s r) = Distribution pdf' q' s' r'
 where
  pdf' x = case (d $ log x) of [pdf] -> pdf/(doubleToLogDouble x)
  q'   = exp . q
  s'   = do v <- Distribution d q s r
            return $ exp v
  r'   = expTransformRange r

log_normal mu sigma = expTransform $ normal mu sigma
log_exponential mu = expTransform $  exponential mu
log_gamma a b = expTransform $ gamma a b
log_laplace m s = expTransform $ laplace m s
log_cauchy m s = expTransform $ cauchy m s
