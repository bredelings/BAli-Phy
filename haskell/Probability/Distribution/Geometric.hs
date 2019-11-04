module Probability.Distribution.Geometric where

import Probability.Random
import MCMC

builtin geometric_density 3 "geometric_density" "Distribution"
builtin builtin_sample_geometric 2 "sample_geometric" "Distribution"

geometric_effect x = add_move (\c -> slice_sample_integer_random_variable x c)

sample_geometric p_success = RandomStructure geometric_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_geometric p_success s)))

geometric2 p_fail p_success = Distribution (make_densities $ geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0)

geometric p = geometric2 (1.0-p) p
rgeometric q = geometric2 q (1.0-q)
