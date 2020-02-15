module Probability.Distribution.Binomial where

import Probability.Random

builtin binomial_density 3 "binomial_density" "Distribution"
builtin builtin_sample_binomial 3 "sample_binomial" "Distribution"
sample_binomial n p = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_binomial n p s)))
binomial n p = Distribution (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (integer_between 0 n)
