module Probability.Distribution.Tuple where

import Probability.Random

data PairDist d1 d2 = PairDist d1 d2

instance (Dist d1, Dist d2) => Dist (PairDist d1 d2) where
    type Result (PairDist d1 d2) = (Result d1, Result d2)
    dist_name (PairDist dist1 dist2) = "PairDist("++dist_name dist1 ++","++dist_name dist2++")"

instance (IOSampleable d1, IOSampleable d2) => IOSampleable (PairDist d1 d2) where
    sampleIO (PairDist dist1 dist2) = do
      x <- sampleIO dist1
      y <- sampleIO dist2
      return (x,y)

instance (HasPdf d1, HasPdf d2) => HasPdf (PairDist d1 d2) where
    pdf (PairDist dist1 dist2) (x1,x2) = pdf dist1 x1 * pdf dist2 x2

instance (Sampleable d1, Sampleable d2) => Sampleable (PairDist d1 d2) where
    sample (PairDist dist1 dist2) = do
      x <- sample dist1
      y <- sample dist2
      return (x,y)
