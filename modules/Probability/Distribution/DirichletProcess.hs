module Probability.Distribution.DirichletProcess where

import Probability.Random

import Probability.Distribution.List
import Probability.Distribution.Normal
import Probability.Distribution.Categorical
    
import MCMC -- for GibbsSampleCategorical

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
                               do c <- sample $ categorical (p alpha counts)
                                  cs <- do_crp'' alpha (n-1) bins (inc counts c) 
                                  return (c:cs)

builtin crp_density 4 "CRP_density" "Distribution"
builtin sample_crp_vector 3 "sample_CRP" "Distribution"
sample_crp alpha n d = RandomStructure modifiable $ liftIO $ do v <- (IOAction3 sample_crp_vector alpha n d)
                                                                return $ list_from_vector v
--crp alpha n d = Distribution (crp_density alpha n d) (no_quantile "crp") (do_crp alpha n d) (ListRange $ replicate n $ integer_between 0 (n+d-1))
modifiable_list_and_pdf density value rv = let raw_list = map modifiable value
                                               trigger_list = map (rv `seq`) raw_list
                                           in (trigger_list, density raw_list)

crp alpha n d = Distribution (make_densities $ density) (no_quantile "crp") (RandomStructureAndPDF (modifiable_list_and_pdf density) (sample_crp alpha n d)) (ListRange $ replicate n subrange)
                  where subrange = integer_between 0 (n+d-1)
                        density = crp_density alpha n d

safe_exp x = if (x < (-20.0)) then
               exp (-20.0)
             else if (x > 20.0) then
               exp 20.0
             else
               exp x

dpm n alpha mean_dist noise_dist= do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist
  sigmaOverMu <- sample $ iid (n+delta) noise_dist

  z <- sample $ iid n (normal 0.0 1.0)

  category <- block $ do category <- random $ sample $ crp alpha n delta
                         add_move (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])
                         return category

  return [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n [0..], let k=category!!i]

dp n alpha mean_dist = do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist

  category <- block $ do category <- random $ sample $ crp alpha n delta
                         add_move (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])
                         return category

  return [ mean!!k | i <- take n [0..], let k=category!!i]
