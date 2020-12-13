module Probability.Distribution.Poisson where

import Probability.Random
import MCMC

builtin poisson_density 2 "poisson_density" "Distribution"
builtin builtin_sample_poisson 2 "sample_poisson" "Distribution"

poisson_bounds = integer_above 0

poisson_effect x = x `seq` bnds `seq` do
   add_move (\c -> slice_sample_integer_random_variable x bnds c)
   add_move (\c -> inc_dec_mh x bnds c)
       where bnds = c_range poisson_bounds

sample_poisson mu = RandomStructure poisson_effect modifiable_structure $ liftIO (IOAction (\s -> (s, builtin_sample_poisson mu s)))

poisson mu = Distribution (make_densities $ poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0)
