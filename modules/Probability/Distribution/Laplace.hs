module Probability.Distribution.Laplace where

import Probability.Random

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 2 "sample_laplace" "Distribution"
sample_laplace m s = RandomStructure do_nothing modifiable $ liftIO (IOAction2 builtin_sample_laplace m s)
laplace m s = Distribution (make_densities $ laplace_density m s) () (sample_laplace m s) realLine
