module Probability.Distribution.Gamma where

import Probability.Random
import MCMC

builtin shifted_gamma_density 4 "shifted_gamma_density" "Distribution"
builtin shifted_gamma_quantile 4 "shifted_gamma_quantile" "Distribution"
builtin builtin_sample_shifted_gamma 4 "sample_shifted_gamma" "Distribution"

gamma_effect x = add_move (\c -> slice_sample_real_random_variable x c)
sample_shifted_gamma a b shift = RandomStructure gamma_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_shifted_gamma a b shift s)))

shifted_gamma a b shift = Distribution (make_densities $ shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (sample_shifted_gamma a b shift) (above shift)
gamma a b = shifted_gamma a b 0.0
