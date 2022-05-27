module Probability.Distribution.Categorical (categorical,categorical_on) where

import Probability.Random
import MCMC

import Foreign.Vector

categorical_effect n x = add_move (\c -> gibbs_sample_categorical x n c)
builtin builtin_sample_categorical 2 "Distribution:sample_categorical"
raw_sample_categorical ps s = builtin_sample_categorical (list_to_vector ps) s
sample_categorical ps = RandomStructure (categorical_effect (length ps)) modifiable_structure $ liftIO (IOAction (\s->(s, raw_sample_categorical ps s)))
-- FIXME: is `make_densities $ qs !` supposed to parse as `make_densities (qs!)`?
annotated_categorical_density qs x = do
  in_edge "qs" qs
  return [qs!x]

categorical ps = Distribution "categorical" (annotated_categorical_density qs) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
                where qs = listArray' $ map doubleToLogDouble ps

sample_categorical_on pairs = do
  i <- categorical $ map snd pairs
  return (fst (pairs!!i))
categorical_on_density pairs x = case lookup x pairs of
                                   Just p -> doubleToLogDouble p
                                   Nothing -> doubleToLogDouble 0.0

annotated_categorical_on_density pairs x = do
  in_edge "pairs" pairs
  return [categorical_on_density pairs x]

categorical_on pairs = Distribution "categorical_on" (annotated_categorical_on_density pairs) (no_quantile "categorical_on") (sample_categorical_on pairs) Nothing
