module Probability (module Probability,

                    module Probability.Random,

                    module Probability.Distribution.Beta,
                    module Probability.Distribution.Cauchy,
                    module Probability.Distribution.Categorical,
                    module Probability.Distribution.Mixture,
                    module Probability.Distribution.Gamma,
                    module Probability.Distribution.Normal,
                    module Probability.Distribution.Dirichlet,
                    module Probability.Distribution.Laplace,
                    module Probability.Distribution.Binomial,
                    module Probability.Distribution.Geometric,
                    module Probability.Distribution.Poisson,
                    module Probability.Distribution.Bernoulli,
                    module Probability.Distribution.Exponential,

                    module Probability.Distribution.Uniform,
                    module Probability.Distribution.Discrete,

                    module Probability.Distribution.List,
                    module Probability.Distribution.Tree
                   )
    where

import Range
import Parameters
import MCMC
import Data.JSON as J
import Tree

import Probability.Random

import Probability.Distribution.Beta
import Probability.Distribution.Cauchy
import Probability.Distribution.Categorical
import Probability.Distribution.Mixture
import Probability.Distribution.Gamma
import Probability.Distribution.Normal
import Probability.Distribution.Dirichlet
import Probability.Distribution.Laplace
import Probability.Distribution.Binomial
import Probability.Distribution.Geometric
import Probability.Distribution.Poisson
import Probability.Distribution.Bernoulli
import Probability.Distribution.Exponential

import Probability.Distribution.Uniform
import Probability.Distribution.Discrete

import Probability.Distribution.List
import Probability.Distribution.Tree

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
modifiable_list = map modifiable
crp alpha n d = Distribution (make_densities $ crp_density alpha n d) (no_quantile "crp") (RandomStructure modifiable_list $ sample_crp alpha n d) (ListRange $ replicate n subrange)
                  where subrange = integer_between 0 (n+d-1)

-- This contains exp-transformed functions
expTransform (Distribution d q s r) = Distribution pdf' q' s' r' 
 where 
  pdf' x = case (d $ log x) of [pdf] -> pdf/(doubleToLogDouble x)
  q'   = exp . q
  s'   = do v <- sample $ Distribution d q s r
            return $ exp v
  r'   = expTransformRange r
  
log_normal mu sigma = expTransform $ normal mu sigma
log_exponential mu = expTransform $  exponential mu
log_gamma a b = expTransform $ gamma a b
log_laplace m s = expTransform $ laplace m s
log_cauchy m s = expTransform $ cauchy m s

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

  category <- Strict $ do category <- Lazy $ sample $ crp alpha n delta
                          AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])
                          return category

  return [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n [0..], let k=category!!i]

dp n alpha mean_dist = do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist

  category <- Strict $ do category <- Lazy $ sample $ crp alpha n delta
                          AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])
                          return category

  return [ mean!!k | i <- take n [0..], let k=category!!i]

