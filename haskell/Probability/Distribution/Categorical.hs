module Probability.Distribution.Categorical (categorical,categorical_on, HasCategorical) where

import Probability.Random
import Data.Array
import Control.Monad.IO.Class
import MCMC

import Foreign.Vector

categorical_effect n x = add_move (\c -> gibbs_sample_categorical x n c)
foreign import bpcall "Distribution:sample_categorical" builtin_sample_categorical :: EVector Double -> RealWorld -> Int
sample_categorical ps = makeIO $ builtin_sample_categorical (list_to_vector ps)
ran_sample_categorical ps = RanAtomic (categorical_effect (length ps)) (sample_categorical ps)

-- FIXME: is `make_densities $ qs !` supposed to parse as `make_densities (qs!)`?
annotated_categorical_density qs x = do
  in_edge "qs" qs
  return [qs!x]

class HasCategorical d where
    categorical :: [Double] -> d Int
    categorical_on :: Eq b => [(b,Double)] -> d b

instance HasCategorical Distribution where
    categorical ps = Distribution "categorical" (annotated_categorical_density qs) (no_quantile "categorical") (ran_sample_categorical ps) (integer_between 0 (length ps - 1))
                where qs = listArray' $ map doubleToLogDouble ps

    categorical_on pairs = Distribution "categorical_on" (annotated_categorical_on_density pairs) (no_quantile "categorical_on") (sample_categorical_on pairs) NoRange

instance HasCategorical Random where
    categorical ps = RanDistribution $ categorical ps
    categorical_on pairs = RanDistribution $ categorical_on pairs

sample_categorical_on :: [(a,Double)] -> Random a
sample_categorical_on pairs = do
  i <- categorical $ map snd pairs :: Random Int
  return (fst (pairs!!i))

categorical_on_density pairs x = case lookup x pairs of
                                   Just p -> doubleToLogDouble p
                                   Nothing -> 0

annotated_categorical_on_density pairs x = do
  in_edge "pairs" pairs
  return [categorical_on_density pairs x]


