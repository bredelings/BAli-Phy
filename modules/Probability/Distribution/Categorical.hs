module Probability.Distribution.Categorical (categorical) where

import Probability.Random
import MCMC

categorical_effect n x = add_move (\c -> gibbs_sample_categorical x n c)
builtin builtin_sample_categorical 2 "sample_categorical" "Distribution"
sample_categorical ps = RandomStructure (categorical_effect (length ps)) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_categorical ps s)))
-- FIXME: is `make_densities $ qs !` supposed to parse as `make_densities (qs!)`?
categorical ps = Distribution (make_densities (qs!)) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
                where qs = listArray' $ map doubleToLogDouble ps
