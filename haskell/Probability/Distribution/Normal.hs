module Probability.Distribution.Normal where

import Probability.Random

builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 3 "sample_normal" "Distribution"
sample_normal m s = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\state -> (state, builtin_sample_normal m s state)))
normal m s = Distribution (make_densities $ normal_density m s) (normal_quantile m s) (sample_normal m s) realLine

