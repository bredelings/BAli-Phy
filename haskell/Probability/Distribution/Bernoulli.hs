module Probability.Distribution.Bernoulli where

import Probability.Random
import MCMC

builtin builtin_sample_bernoulli 2 "sample_bernoulli" "Distribution"

bernoulli_effect x = add_move (\c -> discrete_uniform_avoid_mh x 0 1 c)

sample_bernoulli p = RandomStructure bernoulli_effect modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_bernoulli p s)))

bernoulli_density2 p q 1 = (doubleToLogDouble p)
bernoulli_density2 p q 0 = (doubleToLogDouble q)
bernoulli2 p q = Distribution (make_densities $ bernoulli_density2 p q) (no_quantile "bernoulli") (sample_bernoulli p) (integer_between 0 1)

bernoulli p = bernoulli2 p (1.0-p)
rbernoulli q = bernoulli2 (1.0-q) q
