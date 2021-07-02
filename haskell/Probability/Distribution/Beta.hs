module Probability.Distribution.Beta where

import Probability.Random
import MCMC

builtin beta_density 3 "beta_density" "Distribution"
builtin beta_quantile 3 "beta_quantile" "Distribution"
builtin builtin_sample_beta 3 "sample_beta" "Distribution"

beta_bounds = between 0.0 1.0
beta_effect x = add_move $ slice_sample_real_random_variable x beta_bounds
sample_beta a b = RandomStructure beta_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_beta a b s)))

annotated_beta_density a b x = do
  in_edge "a" a
  in_edge "b" b
  return [beta_density a b x]

beta a b = Distribution "beta" (annotated_beta_density a b) (beta_quantile a b) (sample_beta a b) beta_bounds
