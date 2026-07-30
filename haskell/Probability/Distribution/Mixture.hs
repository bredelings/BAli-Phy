module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Discrete
import Probability.Distribution.Uniform

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

-- Weights are relative and must be nonnegative with a positive total; this
-- interface supplies the two structural operations needed for sampling.
class Dist mixture => WeightedComponents mixture where
    totalWeight :: mixture -> Double
    sampleAt :: Double -> mixture -> Random (Result mixture)

instance Sampleable d => WeightedComponents (Weighted d) where
    totalWeight (Weighted weight _) = weight

    -- A terminal component has no alternative, so its own weight cannot
    -- affect which distribution is sampled after the component is reached.
    sampleAt _ (Weighted _ distribution) = sample distribution

instance (Sampleable d, WeightedComponents rest, Result d ~ Result rest) => WeightedComponents (Mixture2 d rest) where
    totalWeight (Mixture2 (Weighted weight _) rest) = weight + totalWeight rest

    -- The head owns [0,weight); subtracting weight maps every other selector
    -- coordinate into the tail's interval without drawing another selector.
    sampleAt u (Mixture2 (Weighted weight distribution) rest)
        | u < weight = sample distribution
        | otherwise  = sampleAt (u - weight) rest

-- A lone weighted component is selected with probability one, so sampling it
-- delegates directly to its distribution without drawing a selector.
instance Sampleable d => Sampleable (Weighted d) where
    sample (Weighted _ distribution) = sample distribution

-- Draw one selector over the total relative weight, then sample only the
-- component whose half-open interval contains it.
instance (Sampleable d, WeightedComponents rest, Result d ~ Result rest) => Sampleable (Mixture2 d rest) where
    sample mixture = do
        u <- sample $ Uniform 0 (totalWeight mixture)
        sampleAt u mixture

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
