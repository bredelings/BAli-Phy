module Probability.Distribution.Gamma where

import Probability.Random

builtin shifted_gamma_density 4 "shifted_gamma_density" "Distribution"
builtin shifted_gamma_quantile 4 "shifted_gamma_quantile" "Distribution"
builtin builtin_sample_shifted_gamma 4 "sample_shifted_gamma" "Distribution"
sample_shifted_gamma a b shift = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_shifted_gamma a b shift s)))
shifted_gamma a b shift = Distribution (make_densities $ shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (sample_shifted_gamma a b shift) (above shift)
gamma a b = shifted_gamma a b 0.0
