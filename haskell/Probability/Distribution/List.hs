module Probability.Distribution.List where

import Probability.Random
import Data.Unordered

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]


plate n dist_f = independent $ map dist_f [0..n-1]

class HasIndependent d where
    independent :: [d a] -> d [a]
--    independent_on :: [(a,d b)] -> d [(a,b)]
    iid :: Int -> d a -> d [a]

instance HasIndependent Distribution where

    independent dists = Distribution "independent" (make_densities' $ independent_densities dists) (no_quantile "independent") (sequence $ map RanDistribution dists) (ListRange (map distRange dists))

    iid n dist = Distribution iid_name (make_densities' $ independent_densities (replicate n dist)) (no_quantile "iid") iid_sample (ListRange $ take n $ repeat $ distRange dist) where
                             iid_name = "iid "++(dist_name dist)
                             iid_sample = do xs <- RanSamplingRate (1.0/sqrt (intToDouble n)) $ sequence (repeat $ RanDistribution dist)
                                             return $ take n xs


instance HasIndependent Random where
    independent dists = lazy $ sequence dists
--    independent_on dists_pairs = RanDistribution (independent dists_pairs)
    iid n dist = lazy $ do xs <- RanSamplingRate (1.0/sqrt (intToDouble n)) $ sequence $ repeat dist
                           return $ take n xs
