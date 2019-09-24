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
builtin sample_crp_vector 4 "sample_CRP" "Distribution"
sample_crp alpha n d = RandomStructure do_nothing modifiable_structure $ liftIO $ do v <- IOAction (\s->(s,sample_crp_vector alpha n d s))
                                                                                     return $ list_from_vector_of_size v n
--crp alpha n d = Distribution (crp_density alpha n d) (no_quantile "crp") (do_crp alpha n d) (ListRange $ replicate n $ integer_between 0 (n+d-1))
modifiable_list_and_triggered_list value rand_var = let raw_list = map modifiable value
                                                        trigger_list = map (rand_var `seq`) raw_list
                                                    in (raw_list, trigger_list)

crp_effect n d x = add_move (\c -> mapM_ (\l-> gibbs_sample_categorical (x!!l) (n+d) c) [0..n-1])

crp alpha n d = Distribution (make_densities $ density) (no_quantile "crp") (RandomStructure (crp_effect n d) (modifiable_list_and_triggered_list) (sample_crp alpha n d)) (ListRange $ replicate n subrange)
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

  category <- sample $ crp alpha n delta

  return [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n [0..], let k=category!!i]

dp n alpha mean_dist = do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist

  category <- sample $ crp alpha n delta

  return [ mean!!k | i <- take n [0..], let k=category!!i]
