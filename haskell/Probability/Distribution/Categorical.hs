module Probability.Distribution.Categorical where

import Probability.Random
import Data.Array
import Data.Foldable
import MCMC

import Foreign.Vector

categorical_effect n x = add_move $ gibbs_sample_categorical x n
foreign import bpcall "Distribution:sample_categorical" builtin_sample_categorical :: EVector Double -> IO Int
sample_categorical ps = builtin_sample_categorical (array_to_vector ps)

data Categorical = Categorical (Array Int Double)

instance Dist Categorical where
    type Result Categorical = Int
    dist_name _ = "categorical"

instance IOSampleable Categorical where
    sampleIO (Categorical ps) = sample_categorical ps

instance HasPdf Categorical where
    pdf (Categorical ps) n | n < 0          = 0
                           | n >= length ps = 0
                           | otherwise      = doubleToLogDouble $ ps!n

instance Dist1D Categorical where
    cdf (Categorical ps) x | n < 0          = 0
                           | n >= length ps = 1
                           | otherwise      = sum $ take (n+1) $ toList ps
                           where n = floor x

instance HasAnnotatedPdf Categorical where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable Categorical where
    sample dist@(Categorical ps) = RanDistribution2 dist $ categorical_effect (length ps)

categorical ps = Categorical $ listArray' ps

---

data CategoricalOn a = CategoricalOn (Array Int a) (Array Int Double)

instance Dist (CategoricalOn a) where
    type Result (CategoricalOn a) = a
    dist_name _ = "CategoricalOn"

instance IOSampleable (CategoricalOn a) where
    sampleIO (CategoricalOn xs ps) = do
      i <- sample_categorical ps
      return (xs!i)

{-
Error - why doesn't this work?
Also, why  can't we rewrite (Result (CategoricalOn a)) to a?
And, why would we think that xs :: Array Int Int?  Is there some defaulting going on there?     -}

instance Eq a => HasPdf (CategoricalOn a) where
    pdf (CategoricalOn xs ps) x = case elemIndexArray x xs of
                                    Nothing -> 0
                                    Just n -> doubleToLogDouble $ ps!n

instance Eq a => HasAnnotatedPdf (CategoricalOn a) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable (CategoricalOn a) where
    sample dist@(CategoricalOn xs ps) = do
        i <- sample $ Categorical ps
        return $ xs!i


categorical_on pairs = CategoricalOn (listArray' xs) (listArray' ps)
    where xs = map fst pairs
          ps = map snd pairs
