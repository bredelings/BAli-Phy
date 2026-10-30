module Probability.Distribution.Exponential where
import Probability.Random
import Probability.Distribution.Gamma as G

exponential mu = gamma 1 mu
