module Probability.Distribution.Geometric where

import Probability.Random

builtin geometric_density 3 "geometric_density" "Distribution"
builtin builtin_sample_geometric 1 "sample_geometric" "Distribution"
sample_geometric p_success = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_geometric p_success)
geometric2 p_fail p_success = Distribution (make_densities $ geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0)

geometric p = geometric2 (1.0-p) p
rgeometric q = geometric2 q (1.0-q)
