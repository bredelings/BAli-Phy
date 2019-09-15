module Probability.Distribution.Uniform where

import Probability.Random

builtin uniform_density 3 "uniform_density" "Distribution"
builtin builtin_sample_uniform 2 "sample_uniform" "Distribution"
sample_uniform l u = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction2 builtin_sample_uniform l u)
uniform l u = Distribution (make_densities $ uniform_density l u) () (sample_uniform l u) (between l u)

builtin uniform_int_density 3 "uniform_int_density" "Distribution"
builtin builtin_sample_uniform_int 2 "sample_uniform_int" "Distribution"
sample_uniform_int l u = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction2 builtin_sample_uniform_int l u)
uniform_int l u = Distribution (make_densities $ uniform_int_density l u) () (sample_uniform_int l u) (integer_between l u)
