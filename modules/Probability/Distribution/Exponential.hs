module Probability.Distribution.Exponential where
import Probability.Random
import Probability.Distribution.Gamma as G

shifted_exponential mu shift = G.shifted_gamma 1.0 mu shift
exponential mu = shifted_exponential mu 0.0

    
