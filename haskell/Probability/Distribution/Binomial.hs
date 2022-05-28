module Probability.Distribution.Binomial where

import Probability.Random
import MCMC

builtin "Distribution:binomial_density" binomial_density 3
builtin "Distribution:sample_binomial" builtin_sample_binomial 3

binomial_bounds n = integer_between 0 n

binomial_effect n x = do
  add_move $ slice_sample_integer_random_variable x (binomial_bounds n)
  add_move $ inc_dec_mh x (binomial_bounds n)

sample_binomial n p = RandomStructure (binomial_effect n) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_binomial n p s)))

binomial n p = Distribution "binomial" (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (binomial_bounds n)
