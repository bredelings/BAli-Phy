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
    ds <- d
    return $ (1.0/doubleToLogDouble x):ds
  q'   = exp . q
  s'   = do v <- dist
            return $ exp v
  r'   = expTransformRange r
  name' = "log_"++name

log_normal mu sigma = expTransform $ normal mu sigma
log_exponential mu = expTransform $  exponential mu
log_gamma a b = expTransform $ gamma a b
log_laplace m s = expTransform $ laplace m s
log_cauchy m s = expTransform $ cauchy m s
