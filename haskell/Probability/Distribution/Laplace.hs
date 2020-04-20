module Probability.Distribution.Laplace where

import Probability.Random
import MCMC

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 3 "sample_laplace" "Distribution"

laplace_bounds = realLine
laplace_effect x = x `seq` bnds `seq` add_move (\c -> slice_sample_real_random_variable x bnds c) where bnds = c_range laplace_bounds
sample_laplace m s = RandomStructure laplace_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_laplace m s state)))

laplace m s = Distribution (make_densities $ laplace_density m s) () (sample_laplace m s) laplace_bounds
