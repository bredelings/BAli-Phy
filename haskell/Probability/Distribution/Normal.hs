module Probability.Distribution.Normal where

import Probability.Random
import MCMC

builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 3 "sample_normal" "Distribution"

normal_effect x = add_move (\c -> slice_sample_real_random_variable x c)
sample_normal m s = RandomStructure normal_effect modifiable_structure $ liftIO (IOAction (\state -> (state, builtin_sample_normal m s state)))

normal m s = Distribution (make_densities $ normal_density m s) (normal_quantile m s) (sample_normal m s) realLine

