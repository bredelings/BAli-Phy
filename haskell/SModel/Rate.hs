module SModel.Rate where

class Scalable m where
    scaleBy :: Double -> m -> m

class Scalable m => RateModel m where
    rate :: m -> Double

rescale r q = scaleBy (r/rate q) q

