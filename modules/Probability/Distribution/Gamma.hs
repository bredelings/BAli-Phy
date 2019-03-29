module Probability.Distribution.Gamma where

import Probability.Random

-- Define some basic distributions
builtin shifted_gamma_density 4 "shifted_gamma_density" "Distribution"
builtin shifted_gamma_quantile 4 "shifted_gamma_quantile" "Distribution"
builtin builtin_sample_shifted_gamma 3 "sample_shifted_gamma" "Distribution"
sample_shifted_gamma a b shift = RandomStructure modifiable $ liftIO (IOAction3 builtin_sample_shifted_gamma a b shift)
shifted_gamma a b shift = Distribution (make_densities $ shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (sample_shifted_gamma a b shift) (above shift)
gamma a b = shifted_gamma a b 0.0
