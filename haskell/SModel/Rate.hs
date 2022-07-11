module SModel.Rate where

class RateModel m where
    rate :: m -> Double
    scale :: Double -> m -> m
