{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Fractional where

import Compiler.Num
import Compiler.Ratio

infixl 7 /

class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a

    x / y = x * (recip y)
    recip y = 1 / y
    fromRational (Ratio top bottom) = fromInteger top / fromInteger bottom

foreign import ecall "Prelude:" divide_double :: Double -> Double -> Double
foreign import ecall "Prelude:" recip_double :: Double -> Double

instance Fractional Double where
    (/) = divide_double
    recip = recip_double


