module Probability.Distribution.Exponential where
import Probability.Random
import Probability.Distribution.Gamma as G

exponentialDist mu = gammaDist 1 mu
exponential mu = sample $ exponentialDist mu

shifted_exponential mu shift = (shift+) <$> exponential mu

-- sample_exponential mu = sample_gamma 1 mu
-- sample_shifted_exponential mu shift = sample_shifted_gamma 1 mu shift
