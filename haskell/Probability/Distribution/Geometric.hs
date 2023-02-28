module Probability.Distribution.Geometric where

import Probability.Random
import Control.Monad.IO.Class
import MCMC
import Range

foreign import bpcall "Distribution:geometric_density" geometric_density :: Double -> Double -> Int -> LogDouble
foreign import bpcall "Distribution:sample_geometric" builtin_sample_geometric :: Double -> RealWorld -> Int

geometric_bounds = integer_above 0

geometric_effect x = do
  add_move $ slice_sample_integer_random_variable x geometric_bounds
  add_move $ inc_dec_mh x geometric_bounds

sample_geometric p_success = makeIO $ builtin_sample_geometric p_success
ran_sample_geometric p_success = RanAtomic geometric_effect (sample_geometric p_success)

geometric p = geometric2 (1-p) p
rgeometric q = geometric2 q (1-q)

class HasGeometric d where
    geometric2 :: Double -> Double -> d Int

instance HasGeometric Distribution where
    geometric2 p_fail p_success = Distribution "geometric" (make_densities $ geometric_density p_fail p_success) (no_quantile "geometric") (ran_sample_geometric p_success) (integer_above 0)


instance HasGeometric Random where
    geometric2 p_fail p_success = RanDistribution $ geometric2 p_fail p_success
