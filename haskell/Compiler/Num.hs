{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

data Integer

class Num a where
    (+), (-), (*) :: a -> a -> a
    negate, abs, signum :: a -> a
    fromInteger :: Int -> a

    x - y = x + (negate y)
    negate y = 0 - y


foreign import bpcall "Prelude:" add_int :: Int -> Int -> Int
foreign import bpcall "Prelude:" subtract_int :: Int -> Int -> Int
foreign import bpcall "Prelude:" multiply_int :: Int -> Int -> Int
foreign import bpcall "Prelude:" abs_int :: Int -> Int
foreign import bpcall "Prelude:" negate_int :: Int -> Int
foreign import bpcall "Prelude:" signum_int :: Int -> Int

instance Num Int where
    (+) = add_int
    (-) = subtract_int
    (*) = multiply_int
    abs = abs_int
    negate = negate_int
    signum = signum_int
    fromInteger x = x


foreign import bpcall "Prelude:" add_double :: Double -> Double -> Double
foreign import bpcall "Prelude:" subtract_double :: Double -> Double -> Double
foreign import bpcall "Prelude:" multiply_double :: Double -> Double -> Double
foreign import bpcall "Prelude:" abs_double :: Double -> Double
foreign import bpcall "Prelude:" negate_double :: Double -> Double
foreign import bpcall "Prelude:" signum_double :: Double -> Double
foreign import bpcall "Prelude:intToDouble" intToDouble :: Int -> Double

instance Num Double where
    (+) = add_double
    (-) = subtract_double
    (*) = multiply_double
    abs = abs_double
    negate = negate_double
    signum = signum_double
    fromInteger = intToDouble
