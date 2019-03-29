module Probability.Distribution.List where

import Probability.Random

list_densities (d:ds) (x:xs) = densities d x ++ list_densities ds xs
list_densities [] []         = []
list_densities _  _          = [doubleToLogDouble 0.0]

list dists = Distribution (list_densities dists) (no_quantile "list") do_sample (ListRange (map distRange dists))
             where do_sample = SamplingRate (1.0/sqrt (intToDouble $ length dists)) $ mapM sample dists

iid n dist = Distribution (list_densities (replicate n dist)) (no_quantile "iid") iid_sample (ListRange $ take n $ repeat $ distRange dist) where
    iid_sample = do xs <- sample $ list (repeat dist)
                    return $ take n xs

plate n dist_f = list $ map dist_f [0..n-1]
