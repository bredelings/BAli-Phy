module Probability.Distribution.Multinomial where

import Probability.Random
import MCMC
import Probability.Distribution.Binomial
import qualified Data.Vector.Unboxed as U

foreign import trcall "Distribution:multinomial_density" builtin_multinomial_density :: Int -> U.Vector Double -> U.Vector Int -> LogDouble

data Multinomial = Multinomial Int [Double]

instance Dist Multinomial where
    type Result Multinomial = [Int]
    distName _ = "multinomial"

instance IOSampleable Multinomial where
    sampleIO (Multinomial n ps) = sampleIO $ sample_multinomial n ps

instance HasPdf Multinomial where
    pdf (Multinomial n ps) ks = multinomial_density n ps ks

instance HasAnnotatedPdf Multinomial where
    annotatedDensities dist = make_densities $ pdf dist

instance Sampleable Multinomial where
    sample (Multinomial n ps) = sample_multinomial n ps

sample_multinomial n [] = return []
sample_multinomial n (p:ps) = do
  let normalize xs = map (/sum xs) xs
  m <- sample $ binomial n p
  ms <- sample_multinomial (n-m) (normalize ps)
  return (m:ms)

multinomial ps = Multinomial (length ps) ps

multinomial_density n ps xs = builtin_multinomial_density n (U.fromList ps) (U.fromList xs)
