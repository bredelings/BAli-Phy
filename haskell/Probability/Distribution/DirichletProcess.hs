module Probability.Distribution.DirichletProcess where

import Probability.Random
import Control.Monad.IO.Class

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

---

normalize v = map (/total) v where total=sum v

do_crp alpha n d = do_crp'' alpha n bins (replicate bins 0) where bins=n+d
do_crp'' alpha 0 bins counts = return []
do_crp'' alpha n bins counts = let inc (c:cs) 0 = (c+1:cs)
                                   inc (c:cs) i = c:(inc cs (i-1))
                                   p alpha counts = normalize (map f counts)
                                   nzeros = length (filter (==0) counts)
                                   f 0 = alpha/fromIntegral nzeros
                                   f i = fromIntegral i
                               in 
                               do c <- sample $ categorical (p alpha counts)
                                  cs <- do_crp'' alpha (n-1) bins (inc counts c) 
                                  return (c:cs)

foreign import bpcall "Distribution:CRP_density" builtin_crp_density :: Double -> Int -> Int -> EVector Int -> LogDouble
crp_density alpha n d z = builtin_crp_density alpha n d (toVector z)
foreign import bpcall "Distribution:sample_CRP" sample_crp_vector :: Double -> Int -> Int -> IO (EVector Int)
sample_crp alpha n d = do v <- sample_crp_vector alpha n d
                          return $ sizedVectorToList v n
ran_sample_crp alpha n d = liftIO $ sample_crp alpha n d

triggeredModifiableList n value effect = let raw_list = mapn n modifiable value
                                             effect' = unsafePerformIO $ effect raw_list
                                             triggered_list = mapn n (withEffect effect') raw_list
                                         in triggered_list

crp_effect n d x = addMove 1 $ TransitionKernel (\c -> mapM_ (\l-> runTK c $ gibbsSampleCategorical (x!!l) (n+d)) [0..n-1])

safe_exp x = if (x < (-20)) then
               exp (-20)
             else if (x > 20) then
               exp 20
             else
               exp x


data CRP = CRP Double Int Int

instance Dist CRP where
    type Result CRP = [Int]
    dist_name _ = "crp"

instance IOSampleable CRP where
    sampleIO (CRP alpha n d) = sample_crp alpha n d

instance HasPdf CRP where
    pdf (CRP alpha n d) = crp_density alpha n d

instance HasAnnotatedPdf CRP where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable CRP where
    sample dist@(CRP alpha n d) = RanDistribution3 dist (crp_effect n d) (triggeredModifiableList n) (ran_sample_crp alpha n d)

crp = CRP

