module Probability.Distribution.Categorical where

import Probability.Random
import qualified Data.Vector as V
import Data.Foldable
import MCMC

categorical_effect n x = addMove 1 $ gibbsSampleCategorical x n
foreign import bpcall "Distribution:sample_categorical" sampleCategoricalNative :: V.Vector Double -> IO Int
sample_categorical = sampleCategoricalNative

data Categorical = Categorical (V.Vector Double)

instance Dist Categorical where
    type Result Categorical = Int
    dist_name _ = "categorical"

instance IOSampleable Categorical where
    sampleIO (Categorical ps) = sample_categorical ps

instance HasPdf Categorical where
    pdf (Categorical ps) n | n < 0            = 0
                           | n >= V.length ps = 0
                           | otherwise        = doubleToLogDouble $ ps V.! n

instance Dist1D Categorical where
    cdf (Categorical ps) x | n < 0            = 0
                           | n >= V.length ps = 1
                           | otherwise        = sum (V.take (n + 1) ps)
                           where n = floor x

instance HasAnnotatedPdf Categorical where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable Categorical where
    sample dist@(Categorical ps) = RanDistribution2 dist $ categorical_effect (V.length ps)

categorical ps = Categorical $ V.fromList ps

---

data CategoricalOn a = CategoricalOn (V.Vector a) (V.Vector Double)

instance Dist (CategoricalOn a) where
    type Result (CategoricalOn a) = a
    dist_name _ = "CategoricalOn"

instance IOSampleable (CategoricalOn a) where
    sampleIO (CategoricalOn xs ps) = do
      i <- sample_categorical ps
      return (xs V.! i)

{-
Error - why doesn't this work?
Also, why  can't we rewrite (Result (CategoricalOn a)) to a?
And, why would we think that xs :: Vector Int?  Is there some defaulting going on there?     -}

instance Eq a => HasPdf (CategoricalOn a) where
    pdf (CategoricalOn xs ps) x = case V.elemIndex x xs of
                                    Nothing -> 0
                                    Just n -> doubleToLogDouble $ ps V.! n

instance Eq a => HasAnnotatedPdf (CategoricalOn a) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable (CategoricalOn a) where
    sample dist@(CategoricalOn xs ps) = do
        i <- sample $ Categorical ps
        return $ xs V.! i


categorical_on pairs = CategoricalOn (V.fromList xs) (V.fromList ps)
    where xs = map fst pairs
          ps = map snd pairs

-- Materialize the outcomes once, then construct their equal probabilities
-- directly in a boxed vector without an intermediate list.
uniformCategoricalOn xs = CategoricalOn values probabilities
    where values = V.fromList xs
          count = V.length values
          probabilities = V.replicate count (1 / fromIntegral count)
