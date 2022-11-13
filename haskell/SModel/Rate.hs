module SModel.Rate where

class Scalable m where
    scale :: Double -> m -> m

class Scalable m => RateModel m where
    rate :: m -> Double

rescale r q = scale (r/rate q) q

