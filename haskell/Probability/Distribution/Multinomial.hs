module Probability.Distribution.Multinomial where

import Probability.Random
import MCMC
import Probability.Distribution.Binomial

foreign import bpcall "Distribution:multinomial_density" builtin_multinomial_density :: Int -> EVector Double -> EVector Int -> LogDouble

data Multinomial = Multinomial Int [Double]

instance Dist Multinomial where
    type Result Multinomial = [Int]
    dist_name _ = "multinomial"

instance IOSampleable Multinomial where
    sampleIO (Multinomial n ps) = sampleIO $ sample_multinomial n ps

instance HasPdf Multinomial where
    pdf (Multinomial n ps) ks | length ks /= n  = 0
                              | otherwise       = multinomial_density n ps ks

instance HasAnnotatedPdf Multinomial where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable Multinomial where
    sample (Multinomial n ps) = sample_multinomial n ps

multinomial_density n ps ks = builtin_multinomial_density n ps' ks'
    where ps' = toVector ps
          ks' = toVector ks

sample_multinomial n [] = return []
sample_multinomial n (p:ps) = do
  let normalize xs = map (/sum xs) xs
  m <- sample $ binomial n p
  ms <- sample_multinomial (n-m) (normalize ps)
  return (m:ms)

multinomial ps = Multinomial (length ps) ps

