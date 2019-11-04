module Probability.Distribution.Uniform where

import Probability.Random
import MCMC

builtin uniform_density 3 "uniform_density" "Distribution"
builtin builtin_sample_uniform 3 "sample_uniform" "Distribution"

uniform_effect x = add_move (\c -> slice_sample_real_random_variable x c)
sample_uniform l u = RandomStructure uniform_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_uniform l u s)))

uniform l u = Distribution (make_densities $ uniform_density l u) () (sample_uniform l u) (between l u)

builtin uniform_int_density 3 "uniform_int_density" "Distribution"
builtin builtin_sample_uniform_int 3 "sample_uniform_int" "Distribution"

uniform_int_effect x = add_move (\c -> slice_sample_integer_random_variable x c)
sample_uniform_int l u = RandomStructure uniform_int_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_uniform_int l u s)))

uniform_int l u = Distribution (make_densities $ uniform_int_density l u) () (sample_uniform_int l u) (integer_between l u)
