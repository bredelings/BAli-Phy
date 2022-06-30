module Probability.Distribution.Dirichlet where

import Probability.Random
import Probability.Distribution.Gamma
import Probability.Distribution.List

foreign import bpcall "Distribution:dirichlet_density" builtin_dirichlet_density :: EVector Double -> EVector Double -> LogDouble
dirichlet_density as ps = builtin_dirichlet_density (list_to_vector as) (list_to_vector ps)

-- The `dirichlet` does not handle cases where the number of as changes in a ungraceful way: all entries are resampled!
sample_dirichlet as = RanSamplingRate (1.0/sqrt(intToDouble $ length as)) $ do vs <- mapM (\a-> gamma a 1.0) as
                                                                               return $ map (/(sum vs)) vs

-- independent is a Distribution ... so shouldn't this fail because of a lack of a monad instance?

-- The `v_dirichlet` takes an infinite list of as, and a number of them to keep.
sample_v_dirichlet n as = RanSamplingRate (1.0/sqrt(intToDouble n)) $ do vs <- independent [ gamma a 1.0 | a <- as ]
                                                                         let ws = take n vs
                                                                         return $ map (/(sum ws)) ws

v_dirichlet n as = Distribution "v_dirichlet" (make_densities $ dirichlet_density (take n as)) (no_quantile "v_dirichlet") (sample_v_dirichlet n as) (Simplex n 1.0)

symmetric_dirichlet n a = v_dirichlet n (repeat a)

sample_dirichlet_on items as = do ps <- sample_dirichlet as
                                  return $ zip items ps

dirichlet_on_density as item_ps = dirichlet_density as ps where
    ps = map (\(item,p) -> p) item_ps

symmetric_dirichlet_on items a = dirichlet_on items (replicate (length items) a)

class HasDirichlet d where
    dirichlet :: [Double] -> d [Double]
    dirichlet_on :: [a] -> [Double] -> d [(a,Double)]

instance HasDirichlet Distribution where
    dirichlet as = Distribution "dirichlet" (make_densities $ dirichlet_density as) (no_quantile "dirichlet") (sample_dirichlet as) (Simplex (length as) 1.0)
    dirichlet_on items as = Distribution "dirichlet_on" (make_densities $ dirichlet_on_density as) (no_quantile "dirichlet_on") (sample_dirichlet_on items as) NoRange

instance HasDirichlet Random where
    dirichlet as = RanDistribution (dirichlet as)
    dirichlet_on items as = RanDistribution (dirichlet_on items as)
