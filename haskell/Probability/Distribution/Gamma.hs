module Probability.Distribution.Gamma where

import Probability.Random
import MCMC

builtin shifted_gamma_density 4 "shifted_gamma_density" "Distribution"
builtin shifted_gamma_quantile 4 "shifted_gamma_quantile" "Distribution"
builtin builtin_sample_shifted_gamma 4 "sample_shifted_gamma" "Distribution"

gamma_bounds shift = above shift
gamma_effect shift x = add_move $ slice_sample_real_random_variable x (gamma_bounds shift)
sample_shifted_gamma a b shift = RandomStructure (gamma_effect shift) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_shifted_gamma a b shift s)))

annotated_shifted_gamma_density a b shift x = do
  in_edge "a" a
  in_edge "b" b
  in_edge "shift" shift
  return [shifted_gamma_density a b shift x]

shifted_gamma a b shift = Distribution "shifted_gamma" (annotated_shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (sample_shifted_gamma a b shift) (gamma_bounds shift)


---------------------

gamma_density a b = shifted_gamma_density a b 0.0
gamma_quantile a b = shifted_gamma_quantile a b 0.0
sample_gamma a b = sample_shifted_gamma a b 0.0

annotated_gamma_density a b x = do
  in_edge "a" a
  in_edge "b" b
  return [gamma_density a b x]

gamma a b = Distribution "gamma" (annotated_gamma_density a b) (gamma_quantile a b) (sample_gamma a b) (gamma_bounds 0.0)

