module Probability.Distribution.Normal where

import Probability.Random
import MCMC

builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 3 "sample_normal" "Distribution"

normal_bounds = realLine
normal_effect x = add_move $ slice_sample_real_random_variable x normal_bounds
sample_normal m s = RandomStructure normal_effect modifiable_structure $ liftIO (IOAction (\state -> (state, builtin_sample_normal m s state)))

annotated_normal_density mu sigma x = do
  in_edge "mu" mu
  in_edge "sigma" sigma
  return [normal_density mu sigma x]

normal m s = Distribution "normal" (annotated_normal_density m s) (normal_quantile m s) (sample_normal m s) normal_bounds

