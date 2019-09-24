module Probability.Distribution.Laplace where

import Probability.Random

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 3 "sample_laplace" "Distribution"
sample_laplace m s = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_laplace m s state)))
laplace m s = Distribution (make_densities $ laplace_density m s) () (sample_laplace m s) realLine
