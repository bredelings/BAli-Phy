module Probability.Distribution.NegativeBinomial where

import Probability.Random
import MCMC

builtin negative_binomial_density 3 "negative_binomial_density" "Distribution"
builtin builtin_sample_negative_binomial 3 "sample_negative_binomial" "Distribution"

negative_binomial_bounds = integer_above 0

negative_binomial_effect r x = x `seq` bnds `seq` do
    add_move (\c -> slice_sample_integer_random_variable x bnds c)
    add_move (\c -> inc_dec_mh x bnds c)
        where bnds = c_range $ negative_binomial_bounds

sample_negative_binomial r p = RandomStructure (negative_binomial_effect r) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_negative_binomial r p s)))

negative_binomial r p = Distribution (make_densities $ negative_binomial_density r p) (no_quantile "negative_binomial") (sample_negative_binomial r p) (negative_binomial_bounds n)
