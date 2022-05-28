module Probability.Distribution.Geometric where

import Probability.Random
import MCMC
import Range

builtin "Distribution:geometric_density" geometric_density 3
builtin "Distribution:sample_geometric" builtin_sample_geometric 2

geometric_bounds = integer_above 0

geometric_effect x = do
  let bnds = getIntegerBounds geometric_bounds
  add_move $ slice_sample_integer_random_variable x bnds
  add_move $ inc_dec_mh x bnds

sample_geometric p_success = RandomStructure geometric_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_geometric p_success s)))

geometric2 p_fail p_success = Distribution "geometric" (make_densities $ geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0)

geometric p = geometric2 (1.0-p) p
rgeometric q = geometric2 q (1.0-q)
