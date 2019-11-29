module Probability.Distribution.List where

import Probability.Random

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]

-- This cannot handle infinite lists because it uses the total number of distributions to determine the rate.
independent dists = Distribution (independent_densities dists) (no_quantile "independent") do_sample (ListRange (map distRange dists))
             where do_sample = SamplingRate (1.0/sqrt (intToDouble $ length dists)) $ sequence dists

iid n dist = Distribution (independent_densities (replicate n dist)) (no_quantile "iid") iid_sample (ListRange $ take n $ repeat $ distRange dist) where
    iid_sample = do xs <- SamplingRate (1.0/sqrt (intToDouble n)) $ sequence (repeat dist)
                    return $ take n xs

plate n dist_f = independent $ map dist_f [0..n-1]
