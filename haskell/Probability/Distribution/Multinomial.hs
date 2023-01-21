module Probability.Distribution.Multinomial where

import Probability.Random
import MCMC
import Probability.Distribution.Binomial

foreign import bpcall "Distribution:multinomial_density" builtin_multinomial_density :: Int -> EVector Double -> EVector Int -> LogDouble

multinomial_density n ps ks = builtin_multinomial_density n ps' ks'
    where ps' = list_to_vector ps
          ks' = list_to_vector ks

sample_multinomial n [] = return []
sample_multinomial n (p:ps) = do
  let normalize xs = map (/sum xs) xs
  m <- binomial n p
  ms <- sample_multinomial (n-m) (normalize ps)
  return (m:ms)

multinomial n ps = Distribution "multinomial" (make_densities $ multinomial_density n ps) (no_quantile "multinomial") (sample_multinomial n ps) NoRange

-- Not sure this is the same as sorting discrete uniforms.
random_composition' sum n_groups = RanDistribution $ multinomial sum (replicate n_groups (1/fromIntegral n_groups))

-- Not sure this is the same as conditioning on not having any zeros.
random_composition sum n_groups | sum < n_groups = error "random_composition: sum < n_groups!"
                                | otherwise      = map (1+) <$> random_composition' (sum-n_groups) n_groups
