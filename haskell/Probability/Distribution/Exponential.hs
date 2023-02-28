module Probability.Distribution.Exponential where
import Probability.Random
import Probability.Distribution.Gamma as G

shifted_exponential mu shift = G.shifted_gamma 1 mu shift
exponential mu = shifted_exponential mu 0.0
sample_exponential mu = sample_gamma 1 mu
sample_shifted_exponential mu shift = sample_shifted_gamma 1 mu shift
