module Probability.Distribution.List where

import Probability.Random

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]

-- This cannot handle infinite lists because it uses the total number of distributions to determine the rate.
independent dists = Distribution (independent_densities dists) (no_quantile "independent") (sequence dists) (ListRange (map distRange dists))
--  If we try and put a sampling rate on this that depends on the length, then we can't handle infinitely long lists.
--  If we tried to use the sqrt of the number of variables in the list that are instantiated, then that might actually work.

-- If we could make 'independent' work, we could then define `iid n dist = do xs <- independent (repeat dist) ; return $ take n xs`
--  or something like that.
iid n dist = Distribution (independent_densities (replicate n dist)) (no_quantile "iid") iid_sample (ListRange $ take n $ repeat $ distRange dist) where
    iid_sample = do xs <- SamplingRate (1.0/sqrt (intToDouble n)) $ sequence (repeat dist)
                    return $ take n xs

plate n dist_f = independent $ map dist_f [0..n-1]
