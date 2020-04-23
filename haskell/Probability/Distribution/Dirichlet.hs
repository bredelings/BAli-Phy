module Probability.Distribution.Dirichlet where

import Probability.Random
import Probability.Distribution.Gamma

builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution"
dirichlet_density as ps = builtin_dirichlet_density (list_to_vector as) (list_to_vector ps)
sample_dirichlet as = SamplingRate (1.0/sqrt(intToDouble $ length as)) $ do vs <- mapM (\a-> gamma a 1.0) as
                                                                            return $ map (/(sum vs)) vs
dirichlet as = Distribution (make_densities $ dirichlet_density as) (no_quantile "dirichlet") (sample_dirichlet as) (Simplex (length as) 1.0)

dirichlet' n a = dirichlet (replicate n a)

sample_dirichlet_on items as = do ps <- sample_dirichlet as
                                  return $ zip items ps

dirichlet_on_density as item_ps = dirichlet_density as ps where
    ps = map (\(item,p) -> p) item_ps
dirichlet_on items as = Distribution (make_densities $ dirichlet_on_density as) (no_quantile "dirichlet_on") (sample_dirichlet_on items as) (LabelledSimplex items 1.0)
dirichlet_on' items a = dirichlet_on items (replicate (length items) a)

