{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Fractional where

import Compiler.Num

infixl 7 /

class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
-- fromRational :: Rational -> a
    fromRational :: Double -> a

foreign import bpcall "Prelude:" divide_double :: Double -> Double -> Double
foreign import bpcall "Prelude:" recip_double :: Double -> Double

instance Fractional Double where
    (/) = divide_double
    recip = recip_double
    fromRational x = x

foreign import bpcall "Prelude:" divide_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Prelude:" recip_logdouble :: LogDouble -> LogDouble
foreign import bpcall "Prelude:doubleToLogDouble" doubleToLogDouble :: Double -> LogDouble

instance Fractional LogDouble where
    (/) = divide_logdouble
    recip = recip_logdouble
    fromRational = doubleToLogDouble


