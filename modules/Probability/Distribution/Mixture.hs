module Probability.Distribution.Mixture where

import Probability.Random

mixtureRange ((_,dist1):_) = distRange dist1
mixture_density ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixture_density l x)
mixture_density [] _ = (doubleToLogDouble 0.0)
sample_mixture ((p1,dist1):l) = dist1
mixture args = Distribution (make_densities $ mixture_density args) (no_quantile "mixture") (sample_mixture args) (mixtureRange args)
