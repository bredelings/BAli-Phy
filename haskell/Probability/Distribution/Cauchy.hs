module Probability.Distribution.Cauchy where

import Probability.Random
import MCMC

builtin cauchy_density 3 "cauchy_density" "Distribution"
builtin cauchy_quantile 3 "cauchy_quantile" "Distribution"
builtin builtin_sample_cauchy 3 "sample_cauchy" "Distribution"

cauchy_bounds = realLine
cauchy_effect x = x `seq` bnds `seq` add_move (\c -> slice_sample_real_random_variable x bnds c) where bnds = c_range $ cauchy_bounds
sample_cauchy m s = RandomStructure cauchy_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_cauchy m s state)))

cauchy m s = Distribution (make_densities $ cauchy_density m s) (cauchy_quantile m s) (sample_cauchy m s) cauchy_bounds
