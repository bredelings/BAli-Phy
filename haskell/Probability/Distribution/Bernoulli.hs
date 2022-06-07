module Probability.Distribution.Bernoulli where

import Probability.Random
import MCMC

foreign import bpcall "Distribution:sample_bernoulli" builtin_sample_bernoulli :: Double -> RealWorld -> Int

bernoulli_effect x = add_move (\c -> discrete_uniform_avoid_mh x 0 1 c)

sample_bernoulli p = RandomStructure bernoulli_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_bernoulli p s)))

bernoulli_density2 p q 1 = (doubleToLogDouble p)
bernoulli_density2 p q 0 = (doubleToLogDouble q)

annotated_bernoulli_density2 p q x = do
  in_edge "p" p
  return [bernoulli_density2 p q x]

bernoulli p = bernoulli2 p (1.0-p)
rbernoulli q = bernoulli2 (1.0-q) q


class HasBernoulli d where
    bernoulli2 :: Double -> Double -> d Int

instance HasBernoulli Distribution where
    bernoulli2 p q = Distribution "bernoulli" (annotated_bernoulli_density2 p q) (no_quantile "bernoulli") (sample_bernoulli p) (integer_between 0 1)

instance HasBernoulli Random where
    bernoulli2 p q = RanDistribution (bernoulli2 p q)
