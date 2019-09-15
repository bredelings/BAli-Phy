module Probability.Distribution.Binomial where

import Probability.Random

builtin binomial_density 3 "binomial_density" "Distribution"
builtin builtin_sample_binomial 2 "sample_binomial" "Distribution"
sample_binomial n p = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction2 builtin_sample_binomial n p)
binomial n p = Distribution (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (integer_between 0 n)
