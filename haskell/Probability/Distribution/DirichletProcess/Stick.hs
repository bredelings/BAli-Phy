module Probability.Distribution.DirichletProcess.Stick where

-- NOTE: The parent module re-exports this legacy API for existing callers.
-- That compatibility re-export can go once callers migrate here or retire dp/dpm.

import Probability.Random

import Probability.Distribution.List
import Probability.Distribution.Normal
import Probability.Distribution.Bernoulli
import Probability.Distribution.Beta
import Probability.Distribution.Exponential
import Numeric.Log -- for log1p

-- PROBLEM: Does the new stick' yield the same results in the example BES analysis?
--          It seems like stick' allows rather more categories to exist at a time.
--          With the original "stick", its usually just 1 or 2 categories.

-- Select one element from the (possibly infinite) list of values.
-- This version performs `n` bernoulli choices to select category `n`.
stick :: [Double] -> [a] -> Random a
stick (p:ps) (x:xs) = do keep <- sample $ bernoulli p
                         if keep == 1 then
                             return x
                         else
                             stick ps xs

-- This version performs 1 exponential sample to select category n.
--stick' ps xs = exponential 1.0 <&> negate <&> go_log ps xs  where
--    go_log (p:ps) (x:xs) q  = let q' = q - log1p(-p)
--                              in if q' > 0.0
--                                 then x
--                                 else go_log ps xs q'


--
dpm_lognormal n alpha mean_dist noise_dist = dpm n alpha sample_dist
    where sample_dist = do mean <- sample $ mean_dist
                           sigma_over_mu <- sample $ noise_dist
                           let sample_log_normal = do z <- sample $ normal 0 1
                                                      return $ mean*safe_exp (z*sigma_over_mu)
                           return sample_log_normal

-- In theory we could implement `dpm` in terms of `dp`:
--   dpm n alpha sample_dist = sequence $ dp n alpha sample_dist
-- I think the problem with that is that it might not be lazy in n.
-- I need the take to be at the end:
--   liftM (take n) $ sequence $ dp alpha sample_dist

dpm n alpha sample_dist = lazy $ do

  dists  <- sequence $ repeat $ sample $ sample_dist

  breaks <- sequence $ repeat $ sample $ beta 1 alpha

-- stick selects a distribution from the list, and join then samples from the distribution
  sample $ iid n (join $ stick breaks dists)

dp :: Int -> Double -> Random a -> Random [a]
dp n alpha dist = lazy $ do

  atoms  <- sequence $ repeat $ dist

  breaks <- sequence $ repeat $ sample $ beta 1 alpha

  sample $ iid n (stick breaks atoms)

safe_exp x = if (x < (-20)) then
               exp (-20)
             else if (x > 20) then
               exp 20
             else
               exp x
