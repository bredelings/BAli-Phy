module Probability.Distribution.Beta where

import Probability.Random

builtin beta_density 3 "beta_density" "Distribution"
builtin beta_quantile 3 "beta_quantile" "Distribution"
builtin builtin_sample_beta 3 "sample_beta" "Distribution"
sample_beta a b = RandomStructure do_nothing modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_beta a b s)))
beta a b = Distribution (make_densities $ beta_density a b) (beta_quantile a b) (sample_beta a b) (between 0.0 1.0)
