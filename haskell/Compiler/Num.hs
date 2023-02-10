{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

import Compiler.Error

infixl 7 *
infixl 6 +, -

class Num a where
    (+), (-), (*) :: a -> a -> a
    negate, abs, signum :: a -> a
    fromInteger :: Integer -> a

    x - y = x + (negate y)
    negate y = 0 - y


foreign import bpcall "Num:" add_char :: Char -> Char -> Char
foreign import bpcall "Num:" subtract_char :: Char -> Char -> Char
foreign import bpcall "Num:" multiply_char :: Char -> Char -> Char
foreign import bpcall "Num:" abs_char :: Char -> Char
foreign import bpcall "Num:" negate_char :: Char -> Char
foreign import bpcall "Num:" signum_char :: Char -> Char
foreign import bpcall "Num:" integerToChar :: Integer -> Char
foreign import bpcall "Num:" intToChar :: Int -> Char
foreign import bpcall "Num:" charToInt :: Char -> Int

instance Num Char where
    (+) = add_char
    (-) = subtract_char
    (*) = multiply_char
    abs = abs_char
    negate = negate_char
    signum = signum_char
    fromInteger x = integerToChar x


foreign import bpcall "Num:" add_integer :: Integer -> Integer -> Integer
foreign import bpcall "Num:" subtract_integer :: Integer -> Integer -> Integer
foreign import bpcall "Num:" multiply_integer :: Integer -> Integer -> Integer
foreign import bpcall "Num:" abs_integer :: Integer -> Integer
foreign import bpcall "Num:" negate_integer :: Integer -> Integer
foreign import bpcall "Num:" signum_integer :: Integer -> Integer

instance Num Integer where
    (+) = add_integer
    (-) = subtract_integer
    (*) = multiply_integer
    abs = abs_integer
    negate = negate_integer
    signum = signum_integer
    fromInteger x = x


foreign import bpcall "Num:" add_int :: Int -> Int -> Int
foreign import bpcall "Num:" subtract_int :: Int -> Int -> Int
foreign import bpcall "Num:" multiply_int :: Int -> Int -> Int
foreign import bpcall "Num:" abs_int :: Int -> Int
foreign import bpcall "Num:" negate_int :: Int -> Int
foreign import bpcall "Num:" signum_int :: Int -> Int
foreign import bpcall "Num:" integerToInt :: Integer -> Int

instance Num Int where
    (+) = add_int
    (-) = subtract_int
    (*) = multiply_int
    abs = abs_int
    negate = negate_int
    signum = signum_int
    fromInteger = integerToInt


foreign import bpcall "Num:" add_double :: Double -> Double -> Double
foreign import bpcall "Num:" subtract_double :: Double -> Double -> Double
foreign import bpcall "Num:" multiply_double :: Double -> Double -> Double
foreign import bpcall "Num:" abs_double :: Double -> Double
foreign import bpcall "Num:" negate_double :: Double -> Double
foreign import bpcall "Num:" signum_double :: Double -> Double
--foreign import bpcall "Num:" intToDouble :: Int -> Double
foreign import bpcall "Num:" integerToDouble :: Integer -> Double

instance Num Double where
    (+) = add_double
    (-) = subtract_double
    (*) = multiply_double
    abs = abs_double
    negate = negate_double
    signum = signum_double
    fromInteger = integerToDouble

data LogDouble

foreign import bpcall "Num:" add_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" subtract_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" multiply_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" signum_logdouble :: LogDouble -> LogDouble
foreign import bpcall "Num:" integerToLogDouble :: Integer -> LogDouble

instance Num LogDouble where
    (+) = add_logdouble
    (-) = subtract_logdouble
    (*) = multiply_logdouble
    abs x = x
    negate = error "negate LogDouble"
    signum = signum_logdouble
    fromInteger = integerToLogDouble


-- These may get used in other modules...
foreign import bpcall "Num:" intToInteger :: Int -> Integer
foreign import bpcall "Num:" intToDouble :: Int -> Double

