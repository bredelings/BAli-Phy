{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

class Num a where
    (+), (-), (*) :: a -> a -> a
    negate, abs, signum :: a -> a
    fromInteger :: Int -> a

    x - y = x + (negate y)
    negate y = 0 - y


foreign import bpcall "Num:" add_integer :: Integer -> Integer -> Integer
foreign import bpcall "Num:" subtract_integer :: Integer -> Integer -> Integer
foreign import bpcall "Num:" multiply_integer :: Integer -> Integer -> Integer
foreign import bpcall "Num:" abs_integer :: Integer -> Integer
foreign import bpcall "Num:" negate_integer :: Integer -> Integer
foreign import bpcall "Num:" signum_integer :: Integer -> Integer
foreign import bpcall "Num:" integerToInt :: Integer -> Int
foreign import bpcall "Num:" intToInteger :: Int -> Integer

instance Num Integer where
    (+) = add_integer
    (-) = subtract_integer
    (*) = multiply_integer
    abs = abs_integer
    negate = negate_integer
    signum = signum_integer
    fromInteger = intToInteger


foreign import bpcall "Num:" add_int :: Int -> Int -> Int
foreign import bpcall "Num:" subtract_int :: Int -> Int -> Int
foreign import bpcall "Num:" multiply_int :: Int -> Int -> Int
foreign import bpcall "Num:" abs_int :: Int -> Int
foreign import bpcall "Num:" negate_int :: Int -> Int
foreign import bpcall "Num:" signum_int :: Int -> Int

instance Num Int where
    (+) = add_int
    (-) = subtract_int
    (*) = multiply_int
    abs = abs_int
    negate = negate_int
    signum = signum_int
    fromInteger x = x


foreign import bpcall "Num:" add_double :: Double -> Double -> Double
foreign import bpcall "Num:" subtract_double :: Double -> Double -> Double
foreign import bpcall "Num:" multiply_double :: Double -> Double -> Double
foreign import bpcall "Num:" abs_double :: Double -> Double
foreign import bpcall "Num:" negate_double :: Double -> Double
foreign import bpcall "Num:" signum_double :: Double -> Double
foreign import bpcall "Num:" intToDouble :: Int -> Double
foreign import bpcall "Num:" integerToDouble :: Integer -> Double

instance Num Double where
    (+) = add_double
    (-) = subtract_double
    (*) = multiply_double
    abs = abs_double
    negate = negate_double
    signum = signum_double
    fromInteger = intToDouble
