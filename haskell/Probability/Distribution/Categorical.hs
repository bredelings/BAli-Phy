module Probability.Distribution.Categorical where

import Probability.Random
import Data.Array
import Data.Foldable
import Control.Monad.IO.Class
import MCMC

import Foreign.Vector

categorical_effect n x = add_move (\c -> gibbs_sample_categorical x n c)
foreign import bpcall "Distribution:sample_categorical" builtin_sample_categorical :: EVector Double -> RealWorld -> Int
sample_categorical ps = makeIO $ builtin_sample_categorical (array_to_vector ps)

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
    annotated_densities dist n = return [pdf dist n]

instance Sampleable Categorical where
    sample dist@(Categorical ps) = RanDistribution2 dist $ categorical_effect (length ps)

categoricalDist ps = Categorical $ listArray' ps

categorical ps = sample $ categoricalDist ps

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
    annotated_densities dist x = return [pdf dist x]

instance Sampleable (CategoricalOn a) where
    sample dist@(CategoricalOn xs ps) = do
        i <- sample $ Categorical ps
        return $ xs!i


categoricalOnDist pairs = CategoricalOn (listArray' xs) (listArray' ps)
    where xs = map fst pairs
          ps = map snd pairs

categorical_on :: [(a,Double)] -> Random a
categorical_on pairs = sample $ categoricalOnDist pairs

