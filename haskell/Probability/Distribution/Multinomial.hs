module Probability.Distribution.Multinomial where

import Probability.Random
import MCMC
import Probability.Distribution.Binomial
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

foreign import trcall "Distribution:multinomial_density" builtin_multinomial_density
    :: Int -> V.Vector (Log Double) -> U.Vector Int -> Log Double

foreign import trcall "Distribution:multinomial_prob_density" builtin_multinomial_prob_density
    :: Int -> V.Vector (Log Double) -> U.Vector Int -> ProbDensity

data Multinomial = Multinomial Int (V.Vector (Log Double))

instance Dist Multinomial where
    type Result Multinomial = [Int]
    distName _ = "multinomial"

instance IOSampleable Multinomial where
    sampleIO (Multinomial n ps) = sampleIO $ sample_multinomial n ps

instance HasPdf Multinomial where
    pdf (Multinomial n ps) ks = builtin_multinomial_density n ps (U.fromList ks)

instance HasAnnotatedPdf Multinomial where
    annotatedDensities (Multinomial n ps) =
        make_prob_densities $ builtin_multinomial_prob_density n ps . U.fromList

instance Sampleable Multinomial where
    sample (Multinomial n ps) = sample_multinomial n ps

-- Given current mass p and remaining mass r, the conditional success probability p/(p+r)
-- has log odds log(p)-log(r). Suffix sums therefore avoid renormalizing probabilities in Double.
sample_multinomial :: Int -> V.Vector (Log Double) -> Random [Int]
sample_multinomial n probabilities = sampleConditionals n conditionals
  where
    probabilityList = V.toList probabilities
    suffixes = scanr (+) 0 probabilityList
    conditionals :: [(Log Double, Log Double)]
    conditionals = zip probabilityList (tail suffixes)

    -- Once every trial has been assigned, emit zero counts without constructing
    -- undefined conditionals for any trailing zero-mass categories.
    sampleConditionals _ [] = return []
    sampleConditionals 0 remaining = return $ replicate (length remaining) 0
    sampleConditionals remainingCount ((probability, remainingMass):remaining) = do
      count <- sample $ binomial remainingCount $ fromLogOdds (ln probability - ln remainingMass)
      counts <- sampleConditionals (remainingCount-count) remaining
      return (count:counts)

multinomial :: Int -> [Prob] -> Multinomial
multinomial n ps = Multinomial n $ V.fromList $ map toFloating ps

multinomial_density :: Int -> [Prob] -> [Int] -> Log Double
multinomial_density n ps xs =
    builtin_multinomial_density n (V.fromList $ map toFloating ps) (U.fromList xs)
