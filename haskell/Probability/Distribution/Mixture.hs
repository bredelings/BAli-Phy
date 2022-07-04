module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical

mixture_density (p:ps) (dist:dists) x = (doubleToLogDouble p)*(density dist x) + (mixture_density ps dists x)
mixture_density [] [] _ = 0
sample_mixture weights dists = do cat <- RanDistribution (categorical weights)
                                  RanDistribution (dists!!cat)

mixture weights dists = Distribution "mixture"
                                     (make_densities $ mixture_density weights dists)
                                     (no_quantile "mixture")
                                     (sample_mixture weights dists)
                                     NoRange
