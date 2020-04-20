module Probability.Distribution.Binomial where

import Probability.Random
import MCMC

builtin binomial_density 3 "binomial_density" "Distribution"
builtin builtin_sample_binomial 3 "sample_binomial" "Distribution"

binomial_bounds n = integer_between 0 n

binomial_effect n x = x `seq` bnds `seq` add_move (\c -> slice_sample_integer_random_variable x bnds c) where bnds = c_range $ binomial_bounds n

sample_binomial n p = RandomStructure (binomial_effect n) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_binomial n p s)))

binomial n p = Distribution (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (binomial_bounds n)
