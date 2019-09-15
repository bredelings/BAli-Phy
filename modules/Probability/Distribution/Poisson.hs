module Probability.Distribution.Poisson where

import Probability.Random

builtin poisson_density 2 "poisson_density" "Distribution"
builtin builtin_sample_poisson 1 "sample_poisson" "Distribution"
sample_poisson mu = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction1 builtin_sample_poisson mu)
poisson mu = Distribution (make_densities $ poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0)
