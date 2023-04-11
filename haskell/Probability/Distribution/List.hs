module Probability.Distribution.List where

import Probability.Random
import Probability.Distribution.Gamma
import Data.Array
import Effect
import MCMC

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]

plate n dist_f = independent $ map dist_f [0..n-1]

class HasIndependent d where
    independent :: [d a] -> d [a]
--    independent_on :: [(a,d b)] -> d [(a,b)]
    iid :: Int -> d a -> d [a]
    iid_on :: [a] -> d b -> d [(a,b)]

instance HasIndependent Distribution where

    independent dists = Distribution "independent" (make_densities' $ independent_densities dists) (no_quantile "independent") (sequence $ map RanDistribution dists) (ListRange (map distRange dists))

    iid n dist = Distribution iid_name (make_densities' $ independent_densities (replicate n dist)) (no_quantile "iid") iid_sample (ListRange $ take n $ repeat $ distRange dist) where
                             iid_name = "iid "++(dist_name dist)
                             iid_sample = do xs <- RanSamplingRate (1/sqrt (fromIntegral n)) $ sequence (repeat $ RanDistribution dist)
                                             return $ take n xs
    iid_on xs dist = undefined


instance HasIndependent Random where
    independent dists = lazy $ sequence dists
--    independent_on dists_pairs = RanDistribution (independent dists_pairs)
    iid n dist = lazy $ RanSamplingRate (1/sqrt (fromIntegral n)) $ do
                       dist <- interchangeable dist
                       xs <- sequence $ repeat $ dist
                       return $ take n xs

    iid_on vs dist = let n = length vs
                     in lazy $ RanSamplingRate (1/sqrt (fromIntegral n)) $ do
                       dist <- interchangeable dist
                       xs <- sequence $ repeat $ dist
                       return $ zip vs xs

{-
  could we do i.e.
  strict $ do
    sampling_rate (1/sqrt (fromIntegral n)
    xs <- lazy $ sequence $ repeat dist  -- lazy means no effects (add_move) or rate changes (sample_rate)
    add_move $ interchange_list_entries xs  -- ideally this is only allowed within strict blocks!  And MCMC-specific blocks?
    return $ take n xs
-}


iid2 n dist1 dist2 = iid n $ (,) <$> dist1 <*> dist2

iid_mixture_dist n weight_dist item_dist = do
    (ps, items) <- unzip <$> iid2 n weight_dist item_dist
    return (map (/sum ps) ps, items)

dirichlet_mixture_dist n a item_dist = iid_mixture_dist n (gamma a 1) item_dist


even_sorted_on_iid f n dist = do let n_all = 2*n+1
                                 xs' <- iid n_all dist
                                 let xs = listArray n_all $ sortOn f xs'
                                 return [xs!(2*i-1) | i <- [1..n]]

even_sorted_iid = even_sorted_on_iid id


odd_sorted_on_iid f n dist = do let n_all = max (2*n-1) 0
                                xs' <- iid n_all dist
                                let xs = listArray n_all $ sortOn f xs'
                                return [xs!(2*i) | i <- [0..n-1]]

odd_sorted_iid = odd_sorted_on_iid id
{-
odd_iid_mixture_dist n weight_dist item_dist = do
    (ps, items) <- unzip <$> odd_sorted_on_iid snd n $ (,) <$> weight_dist <*> item_dist
    return (map (/sum ps) ps, items)

even_iid_mixture_dist n weight_dist item_dist = do
    (ps, items) <- unzip <$> even_sorted_on_iid snd n $ (,) <$> weight_dist <*> item_dist
    return (map (/sum ps) ps, items)

dirichlet_odd_mixture_dist  n a item_dist = odd_iid_mixture_dist  n (gamma a 1) item_dist
dirichlet_even_mixture_dist n a item_dist = even_iid_mixture_dist n (gamma a 1) item_dist
-}

{-
  So, the need to couple the two distributions is very inconvenient!
  It would be much easier if we could generate the dirichlet separately,
  ... but that is inherently using the index of the weight and the item to
  decide which items and weights correspond!
-}
