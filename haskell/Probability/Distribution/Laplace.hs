module Probability.Distribution.Laplace where

import Probability.Random
import MCMC

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 3 "sample_laplace" "Distribution"

laplace_effect x = add_move (\c -> slice_sample_real_random_variable x c)
sample_laplace m s = RandomStructure laplace_effect modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_laplace m s state)))

laplace m s = Distribution (make_densities $ laplace_density m s) () (sample_laplace m s) realLine
