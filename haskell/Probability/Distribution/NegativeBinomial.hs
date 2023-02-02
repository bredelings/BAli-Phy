module Probability.Distribution.NegativeBinomial where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:negative_binomial_density" negative_binomial_density :: () -> () -> () -> ()
foreign import bpcall "Distribution:sample_negative_binomial" builtin_sample_negative_binomial :: () -> () -> () -> ()

negative_binomial_bounds = integer_above 0

negative_binomial_effect r x = do
    add_move $ slice_sample_integer_random_variable x negative_binomial_bounds
    add_move $ inc_dec_mh x negative_binomial_bounds

sample_negative_binomial r p = RanAtomic (negative_binomial_effect r) (IOAction (\s->(s,builtin_sample_negative_binomial r p s)))

negative_binomial r p = Distribution "negative_binomial" (make_densities $ negative_binomial_density r p) (no_quantile "negative_binomial") (sample_negative_binomial r p) (negative_binomial_bounds n)
