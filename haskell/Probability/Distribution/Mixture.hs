module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical
import Probability.Distribution.Discrete
import qualified Data.Vector as V

-- `Weighted` terminates a heterogeneous mixture, while `Mixture2` adds a
-- component without erasing the concrete distribution types in the tail.
data Weighted d = Weighted Double d
data Mixture2 d rest = Mixture2 (Weighted d) rest

infix 5 .*.
(.*.) :: Double -> d -> Weighted d
(.*.) = Weighted

infixr 4 |+|
(|+|) :: Weighted d -> rest -> Mixture2 d rest
(|+|) = Mixture2

instance Dist d => Dist (Weighted d) where
    type Result (Weighted d) = Result d
    distName _ = "weighted"

instance (Dist d, Dist rest, Result d ~ Result rest) => Dist (Mixture2 d rest) where
    type Result (Mixture2 d rest) = Result d
    distName _ = "mixture2"

-- Composite-mixture weights are relative and must be nonnegative with a
-- positive total; collecting them does not require a sampling capability.
class Dist mixture => WeightedComponents mixture where
    componentWeights :: mixture -> [Double]

instance Dist d => WeightedComponents (Weighted d) where
    componentWeights (Weighted weight _) = [weight]

instance (Dist d, WeightedComponents rest, Result d ~ Result rest) => WeightedComponents (Mixture2 d rest) where
    componentWeights (Mixture2 (Weighted weight _) rest) =
        weight : componentWeights rest

-- NOTE: These capability-specific traversals can become one rank-n
-- `AllComponents capability mixture` fold once the compiler supports
-- ConstraintKinds and variable-headed predicates in kind checking and the
-- solver. RankNTypes and ordinary `Result` equality already suffice otherwise.
class (WeightedComponents mixture, Ord (Result mixture)) => Dist1DComponents mixture where
    weightedCdfSummary :: mixture -> Double -> (Double, Double)
    componentsLowerBound :: mixture -> Maybe (Result mixture)
    componentsUpperBound :: mixture -> Maybe (Result mixture)

instance Dist1D d => Dist1DComponents (Weighted d) where
    weightedCdfSummary (Weighted weight distribution) x =
        (weight, weight * cdf distribution x)
    componentsLowerBound (Weighted _ distribution) = lower_bound distribution
    componentsUpperBound (Weighted _ distribution) = upper_bound distribution

instance (Dist1D d, Dist1DComponents rest, Result d ~ Result rest) =>
    Dist1DComponents (Mixture2 d rest) where
    -- Retain the unnormalized numerator and total weight so a zero-weight
    -- suffix is neutral instead of requiring an undefined intermediate CDF.
    weightedCdfSummary (Mixture2 (Weighted weight distribution) rest) x =
        (weight + restWeight, weight * cdf distribution x + restNumerator)
      where
        (restWeight, restNumerator) = weightedCdfSummary rest x
    componentsLowerBound (Mixture2 (Weighted _ distribution) rest) =
        min <$> lower_bound distribution <*> componentsLowerBound rest
    componentsUpperBound (Mixture2 (Weighted _ distribution) rest) =
        max <$> upper_bound distribution <*> componentsUpperBound rest

instance Dist1D d => Dist1D (Weighted d) where
    cdf (Weighted _ distribution) = cdf distribution
    lower_bound (Weighted _ distribution) = lower_bound distribution
    upper_bound (Weighted _ distribution) = upper_bound distribution

instance (Dist1D d, Dist1DComponents rest, Result d ~ Result rest) =>
    Dist1D (Mixture2 d rest) where
    -- A mixture CDF is the weighted numerator divided by the total weight;
    -- normalizing only here avoids division by zero in an inactive suffix.
    cdf mixture x = numerator / total
      where
        (total, numerator) = weightedCdfSummary mixture x
    lower_bound = componentsLowerBound
    upper_bound = componentsUpperBound

-- Walk to one indexed component and apply `sample` only to that distribution.
class WeightedComponents mixture => SampleableComponents mixture where
    sampleComponent :: Int -> mixture -> Random (Result mixture)

instance Sampleable d => SampleableComponents (Weighted d) where
    sampleComponent 0 (Weighted _ distribution) = sample distribution
    sampleComponent _ _ = error "sampleComponent: component index out of range"

instance (Sampleable d, SampleableComponents rest, Result d ~ Result rest) => SampleableComponents (Mixture2 d rest) where
    -- Index zero selects the head; positive indices are shifted into the
    -- zero-based component index of the tail.
    sampleComponent 0 (Mixture2 (Weighted _ distribution) _) = sample distribution
    sampleComponent index (Mixture2 _ rest)
        | index > 0 = sampleComponent (index - 1) rest
        | otherwise = error "sampleComponent: component index out of range"

-- A lone weighted component is selected with probability one, so sampling it
-- delegates directly to its distribution without drawing a selector.
instance Sampleable d => Sampleable (Weighted d) where
    sample (Weighted _ distribution) = sample distribution

instance (Sampleable d, SampleableComponents rest, Result d ~ Result rest) => Sampleable (Mixture2 d rest) where
    -- Normalize the relative weights, choose one categorical index, and sample
    -- only the distribution at that index.
    sample mixture = do
        let weights = componentWeights mixture
            total = sum weights
            probabilities = V.map (/ total) (V.fromList weights)
        index <- sample $ Categorical probabilities
        -- Categorical always returns an index covered by `componentWeights`.
        sampleComponent index mixture

newtype Mixture d = Mixture d

instance (Dist d, Dist (Result d)) => Dist (Mixture d) where
    type Result (Mixture d) = Result (Result d)
    distName _ = "mixture"

instance (IOSampleable d, IOSampleable (Result d)) => IOSampleable (Mixture d) where
    sampleIO (Mixture dist1) = do dist2 <- sampleIO dist1
                                  sampleIO dist2

instance (Sampleable d, Sampleable (Result d)) => Sampleable (Mixture d) where
    sample (Mixture dist1) = do dist2 <- sample dist1
                                sample dist2

instance HasPdf d => HasPdf (Mixture (Discrete d)) where
    pdf (Mixture (Discrete pairs)) x = sum [ doubleToLogDouble p * pdf dist x | (dist,p) <- pairs ]

instance HasAnnotatedPdf d => HasAnnotatedPdf (Mixture (Discrete d)) where
    annotatedDensities (Mixture (Discrete pairs)) x = return $ ([sum [doubleToLogDouble p * density dist x | (dist,p) <- pairs]], ())


mixture ps dists | length ps /= length dists  = error "mixture distribution has different number of weights and distributions"
                 | otherwise                  = Mixture $ Discrete $ zip dists ps 

equalMixture dists = mixture ps dists where
    n = length dists
    ps = replicate n (1/fromIntegral n)
