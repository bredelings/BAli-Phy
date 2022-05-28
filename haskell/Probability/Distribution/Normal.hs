module Probability.Distribution.Normal where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:normal_density" normal_density :: () -> () -> () -> ()
foreign import bpcall "Distribution:normal_quantile" normal_quantile :: () -> () -> () -> ()
foreign import bpcall "Distribution:sample_normal" builtin_sample_normal :: () -> () -> () -> ()

normal_bounds = realLine
normal_effect x = add_move $ slice_sample_real_random_variable x normal_bounds
sample_normal m s = RandomStructure normal_effect modifiable_structure $ liftIO (IOAction (\state -> (state, builtin_sample_normal m s state)))

annotated_normal_density mu sigma x = do
  in_edge "mu" mu
  in_edge "sigma" sigma
  return [normal_density mu sigma x]

normal m s = Distribution "normal" (annotated_normal_density m s) (normal_quantile m s) (sample_normal m s) normal_bounds

