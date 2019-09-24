module Probability.Distribution.Cauchy where

import Probability.Random

builtin cauchy_density 3 "cauchy_density" "Distribution"
builtin builtin_sample_cauchy 3 "sample_cauchy" "Distribution"
sample_cauchy m s = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\state->(state,builtin_sample_cauchy m s state)))
cauchy m s = Distribution (make_densities $ cauchy_density m s) () (sample_cauchy m s) realLine
