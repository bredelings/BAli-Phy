module Probability.Distribution.List where

import Probability.Random

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]

-- This cannot handle infinite lists because it uses the total number of distributions to determine the rate.
independent dists = Distribution (independent_densities dists) (no_quantile "independent") (sequence dists) (ListRange (map distRange dists))
--  If we try and put a sampling rate on this that depends on the length, then we can't handle infinitely long lists.
--  If we tried to use the sqrt of the number of variables in the list that are instantiated, then that might actually work.

independent_densities_on dist_pairs obs_pairs
    | length dist_pairs == length obs_pairs    = go dist_pairs obs_pairs
    | otherwise                                = [doubleToLogDouble 0.0]
    where go [] _ = []
          go ((key,dist):rest) obs_pairs = case lookup key obs_pairs of
                                             Just obs -> densities dist obs ++ go rest obs_pairs
                                             Nothing -> doubleToLogDouble 0.0
sample_independent_on [] = return []
sample_independent_on ((key,dist):rest) = do x <- dist
                                             pairs <- sample_independent_on rest
                                             return ((key,x):pairs)

independant_on dists_pairs = Distribution (independent_densities_on dists_pairs) (no_quantile "independent_on") (sample_independent_on dists_pairs) Nothing


-- If we could make 'independent' work, we could then define `iid n dist = do xs <- independent (repeat dist) ; return $ take n xs`
--  or something like that.
iid n dist = Distribution (independent_densities (replicate n dist)) (no_quantile "iid") iid_sample (ListRange $ take n $ repeat $ distRange dist) where
    iid_sample = do xs <- SamplingRate (1.0/sqrt (intToDouble n)) $ sequence (repeat dist)
                    return $ take n xs

iid_on keys dist = independant_on (zip keys (repeat dist))

plate n dist_f = independent $ map dist_f [0..n-1]
