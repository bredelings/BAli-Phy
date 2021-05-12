module Probability.Distribution.Multinomial where

import Probability.Random
import MCMC
import Probability.Distribution.Binomial

builtin builtin_multinomial_density 3 "multinomial_density" "Distribution"

multinomial_density n ps ks = builtin_multinomial_density n ps' ks'
    where ps' = list_to_vector ps
          ks' = list_to_vector ks

sample_multinomial n [] = return []
sample_multinomial n (p:ps) = do
  let normalize xs = map (/sum xs) xs
  m <- binomial n p
  ms <- sample_multinomial (n-m) (normalize ps)
  return (m:ms)

multinomial n ps = Distribution (make_densities $ multinomial_density n ps) (no_quantile "multinomial") (sample_multinomial n ps) ()

random_composition sum n_groups  = multinomial sum (replicate n_groups (1.0/intToDouble n_groups))
