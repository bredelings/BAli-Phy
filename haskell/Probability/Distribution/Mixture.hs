module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical

mixtureRange _ (d:ds) = distRange
mixture_density (p:ps) (dist:dists) x = (doubleToLogDouble p)*(density dist x) + (mixture_density ps dists x)
mixture_density [] _ x = (doubleToLogDouble 0.0)
sample_mixture weights dists = do cat <- categorical weights
                                  dists!!cat

mixture weights dists = Distribution (make_densities $ mixture_density weights dists)
                                     (no_quantile "mixture")
                                     (sample_mixture weights dists)
                                     (mixtureRange weights dists)
