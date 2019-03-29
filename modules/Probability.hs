module Probability (module Probability,
                    module Probability.Random,
                    module Probability.Distribution.Beta,
                    module Probability.Distribution.Gamma,
                    module Probability.Distribution.Normal,
                    module Probability.Distribution.Uniform,
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
import Probability.Distribution.Gamma
import Probability.Distribution.Normal
import Probability.Distribution.Uniform

import Probability.Distribution.List
import Probability.Distribution.Tree

builtin cauchy_density 3 "cauchy_density" "Distribution"
builtin builtin_sample_cauchy 2 "sample_cauchy" "Distribution"
sample_cauchy m s = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_cauchy m s)
cauchy m s = Distribution (make_densities $ cauchy_density m s) () (sample_cauchy m s) realLine

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 2 "sample_laplace" "Distribution"
sample_laplace m s = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_laplace m s)
laplace m s = Distribution (make_densities $ laplace_density m s) () (sample_laplace m s) realLine

builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution"
dirichlet_density ns ps = builtin_dirichlet_density (list_to_vector ns) (list_to_vector ps)
sample_dirichlet ns = SamplingRate (1.0/sqrt(intToDouble $ length ns)) $ do vs <- mapM (\a-> sample $ gamma a 1.0) ns
                                                                            return $ map (/(sum vs)) vs
dirichlet ns = Distribution (make_densities $ dirichlet_density ns) (no_quantile "dirichlet") (sample_dirichlet ns) (Simplex (length ns) 1.0)

dirichlet' l n = dirichlet (replicate l n)

sample_dirichlet_on xs ns = do ps <- sample_dirichlet ns
                               return $ zip xs ps

dirichlet_on_density ns xps = dirichlet_density ns ps where
    ps = map (\(x,p) -> p) xps
dirichlet_on xs ns = Distribution (make_densities $ dirichlet_on_density ns) (no_quantile "dirichlet_on") (sample_dirichlet_on xs ns) (LabelledSimplex xs 1.0)
dirichlet_on' xs n = dirichlet_on xs (replicate (length xs) n)

builtin binomial_density 3 "binomial_density" "Distribution"
builtin builtin_sample_binomial 2 "sample_binomial" "Distribution"
sample_binomial n p = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_binomial n p)
binomial n p = Distribution (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (integer_between 0 n)

-- A geometric distribution on [0,\infty).  How many failures before a success?
builtin geometric_density 3 "geometric_density" "Distribution"
builtin builtin_sample_geometric 1 "sample_geometric" "Distribution"
sample_geometric p_success = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_geometric p_success)
geometric2 p_fail p_success = Distribution (make_densities $ geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0)

geometric p = geometric2 (1.0-p) p
rgeometric q = geometric2 q (1.0-q)

builtin poisson_density 2 "poisson_density" "Distribution"
builtin builtin_sample_poisson 1 "sample_poisson" "Distribution"
sample_poisson mu = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_poisson mu)
poisson mu = Distribution (make_densities $ poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0)

builtin builtin_sample_bernoulli 1 "sample_bernoulli" "Distribution"
sample_bernoulli p = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_bernoulli p)
bernoulli_density2 p q 1 = (doubleToLogDouble p)
bernoulli_density2 p q 0 = (doubleToLogDouble q)
bernoulli2 p q = Distribution (make_densities $ bernoulli_density2 p q) (no_quantile "bernoulli") (sample_bernoulli p) (integer_between 0 1)

bernoulli p = bernoulli2 p (1.0-p)
rbernoulli q = bernoulli2 (1.0-q) q

shifted_exponential mu shift = shifted_gamma 1.0 mu shift
exponential mu = shifted_exponential mu 0.0

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

mixtureRange ((_,dist1):_) = distRange dist1
mixture_density ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixture_density l x)
mixture_density [] _ = (doubleToLogDouble 0.0)
sample_mixture ((p1,dist1):l) = dist1
mixture args = Distribution (make_densities $ mixture_density args) (no_quantile "mixture") (sample_mixture args) (mixtureRange args)

builtin builtin_sample_categorical 1 "sample_categorical" "Distribution"
sample_categorical ps = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_categorical ps)
categorical ps = Distribution (make_densities $ qs!) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
                where qs = listArray' $ map doubleToLogDouble ps

-- This contains functions for working with DiscreteDistribution

uniformQuantiles q n = map (\i -> q ((2.0*(intToDouble i)+1.0)/(intToDouble n)) ) (take n [1..])

mix fs ds = [(p*f, x) | (f, d) <- zip' fs ds, (p, x) <- d]

certainly x = [(1.0, x)]

extendDiscreteDistribution d p x = mix [p, 1.0-p] [certainly x, d]

average l = foldl' (\x y->(x+(fst y)*(snd y))) 0.0 l

uniformGrid n = [( 1.0/n', (2.0*i'+1.0)/(2.0*n') ) | i <- take n [0..], let n' = intToDouble n, let i'=intToDouble i]

uniformDiscretize dist n = [(p, quantile dist x) | (p,x) <- uniformGrid n]

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

