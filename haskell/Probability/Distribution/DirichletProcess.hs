module Probability.Distribution.DirichletProcess where

import Probability.Random

import Probability.Distribution.List
import Probability.Distribution.Normal
import Probability.Distribution.Bernoulli
import Probability.Distribution.Beta
import Probability.Distribution.Categorical
import Probability.Distribution.Exponential
import Probability.Distribution.Uniform
import Numeric.Log -- for log1p

import Foreign.Vector

import Control.DeepSeq
import MCMC -- for GibbsSampleCategorical

-- PROBLEM: Does the new stick' yield the same results in the example BES analysis?
--          It seems like stick' allows rather more categories to exist at a time.
--          With the original "stick", its usually just 1 or 2 categories.

-- Select one element from the (possibly infinite) list of values.
-- This version performs `n` bernoulli choices to select category `n`.
stick :: [Double] -> [a] -> Random a
stick (p:ps) (x:xs) = do keep <- bernoulli p
                         if keep == 1 then
                             return x
                         else
                             stick ps xs

stick_dist ps xs = Distribution "stick" (error "no density") (error "no quantile") (stick ps xs) NoRange

-- This version performs 1 exponential sample to select category n.
--stick' ps xs = exponential 1.0 <&> negate <&> go_log ps xs  where
--    go_log (p:ps) (x:xs) q  = let q' = q - log1p(-p)
--                              in if q' > 0.0
--                                 then x
--                                 else go_log ps xs q'


normalize v = map (/total) v where total=sum v

do_crp alpha n d = do_crp'' alpha n bins (replicate bins 0) where bins=n+d
do_crp'' alpha 0 bins counts = return []
do_crp'' alpha n bins counts = let inc (c:cs) 0 = (c+1:cs)
                                   inc (c:cs) i = c:(inc cs (i-1))
                                   p alpha counts = normalize (map f counts)
                                   nzeros = length (filter (==0) counts)
                                   f 0 = alpha/(intToDouble nzeros)
                                   f i = intToDouble i
                               in 
                               do c <- categorical (p alpha counts)
                                  cs <- do_crp'' alpha (n-1) bins (inc counts c) 
                                  return (c:cs)

foreign import bpcall "Distribution:CRP_density" builtin_crp_density :: Double -> Int -> Int -> EVector Int -> LogDouble
crp_density alpha n d z = builtin_crp_density alpha n d (list_to_vector z)
foreign import bpcall "Distribution:sample_CRP" sample_crp_vector :: Double -> Int -> Int -> RealWorld -> EVector Int
sample_crp alpha n d = RandomStructure do_nothing modifiable_structure $ liftIO $ do v <- IOAction (\s->(s,sample_crp_vector alpha n d s))
                                                                                     return $ list_from_vector_of_size v n
--crp alpha n d = Distribution "crp" (make_densities $ crp_density alpha n d) (no_quantile "crp") (do_crp alpha n d) (ListRange $ replicate n $ integer_between 0 (n+d-1))
triggered_modifiable_list n value effect = let raw_list = mapn n modifiable value
                                               effect' = force_list raw_list `seq` effect
                                               triggered_list = mapn n (effect' `seq`) raw_list
                                                    in (raw_list, triggered_list)

crp_effect n d x = add_move (\c -> mapM_ (\l-> gibbs_sample_categorical (x!!l) (n+d) c) [0..n-1])

crp alpha n d = Distribution "crp" (make_densities $ density) (no_quantile "crp") (RandomStructure (crp_effect n d) (triggered_modifiable_list n) (sample_crp alpha n d)) (ListRange $ replicate n subrange)
                  where subrange = integer_between 0 (n+d-1)
                        density = crp_density alpha n d

safe_exp x = if (x < (-20.0)) then
               exp (-20.0)
             else if (x > 20.0) then
               exp 20.0
             else
               exp x

--dpm_lognormal n alpha mean_dist noise_dist = dpm n alpha sample_dist
--    where sample_dist = do mean <- mean_dist
--                           sigma_over_mu <- noise_dist
--                           let sample_log_normal = do z <- normal 0.0 1.0
--                                                      return $ mean*safe_exp (z*sigma_over_mu)
--                           return sample_log_normal

-- In theory we could implement `dpm` in terms of `dp`:
--   dpm n alpha sample_dist = sequence $ dp n alpha sample_dist
-- I think the problem with that is that it might not be lazy in n.
-- I need the take to be at the end:
--   liftM (take n) $ sequence $ dp alpha sample_dist

--dpm :: Int -> Double -> Distribution (Distribution a)
--dpm n alpha sample_dist = lazy $ do

--  dists  <- sequence $ repeat $ RanDistribution sample_dist

--  breaks <- sequence $ repeat $ beta 1.0 alpha

-- stick selects a distribution from the list, and join then samples from the distribution
-- here we really want 
--  iid n (join $ RanDistribution $ stick_dist breaks dists)

dp :: Int -> Double -> Random a -> Random [a]
dp n alpha dist = lazy $ do

  atoms  <- sequence $ repeat $ dist

  -- We need the Random Double here to avoid an ambiguous (HasBeta d) constraint.
  breaks <- sequence $ repeat $ (beta 1.0 alpha :: Random Double)

  iid n (stick_dist breaks atoms)
