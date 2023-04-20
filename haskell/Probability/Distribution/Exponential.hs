module Probability.Distribution.Exponential where
import Probability.Random
import Probability.Distribution.Gamma as G

import Probability.Distribution.Shifted

exponential mu = gamma 1 mu

shifted_exponential mu shift = Shifted (exponential mu) shift

-- sample_exponential mu = sample_gamma 1 mu
-- sample_shifted_exponential mu shift = sample_shifted_gamma 1 mu shift
