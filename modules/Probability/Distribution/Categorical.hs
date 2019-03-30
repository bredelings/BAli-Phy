module Probability.Distribution.Categorical where

import Probability.Random

builtin builtin_sample_categorical 1 "sample_categorical" "Distribution"
sample_categorical ps = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_categorical ps)
categorical ps = Distribution (make_densities $ qs!) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
                where qs = listArray' $ map doubleToLogDouble ps
