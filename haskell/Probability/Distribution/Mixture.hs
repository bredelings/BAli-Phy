module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical

mixture_density (p:ps) (dist:dists) x = (doubleToLogDouble p)*(density dist x) + (mixture_density ps dists x)
mixture_density []     []           _ = 0

sample_mixture :: [Double] -> [Distribution a] -> Random a
sample_mixture weights dists = do cat <- categorical weights
                                  RanDistribution $ dists !! cat

class HasMixture d where
    mixture :: [Double] -> [d a] -> d a

instance HasMixture Distribution where
    mixture weights dists = Distribution "mixture"
                                         (make_densities $ mixture_density weights dists)
                                         (no_quantile "mixture")
                                         (sample_mixture weights dists)
                                         NoRange
instance HasMixture Random where
    mixture weights dists = do cat <- categorical weights
                               dists !! cat








