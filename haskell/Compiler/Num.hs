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


foreign import ecall "Num:" add_char :: Char -> Char -> Char
foreign import ecall "Num:" subtract_char :: Char -> Char -> Char
foreign import ecall "Num:" multiply_char :: Char -> Char -> Char
foreign import ecall "Num:" abs_char :: Char -> Char
foreign import ecall "Num:" negate_char :: Char -> Char
foreign import ecall "Num:" signum_char :: Char -> Char
foreign import ecall "Num:" integerToChar :: Integer -> Char
foreign import ecall "Num:" intToChar :: Int -> Char
foreign import ecall "Num:" charToInt :: Char -> Int

instance Num Char where
    (+) = add_char
    (-) = subtract_char
    (*) = multiply_char
    abs = abs_char
    negate = negate_char
    signum = signum_char
    fromInteger x = integerToChar x


foreign import ecall "Num:" add_integer :: Integer -> Integer -> Integer
foreign import ecall "Num:" subtract_integer :: Integer -> Integer -> Integer
foreign import ecall "Num:" multiply_integer :: Integer -> Integer -> Integer
foreign import ecall "Num:" abs_integer :: Integer -> Integer
foreign import ecall "Num:" negate_integer :: Integer -> Integer
foreign import ecall "Num:" signum_integer :: Integer -> Integer

instance Num Integer where
    (+) = add_integer
    (-) = subtract_integer
    (*) = multiply_integer
    abs = abs_integer
    negate = negate_integer
    signum = signum_integer
    fromInteger x = x


foreign import ecall "Num:" add_int :: Int -> Int -> Int
foreign import ecall "Num:" subtract_int :: Int -> Int -> Int
foreign import ecall "Num:" multiply_int :: Int -> Int -> Int
foreign import ecall "Num:" abs_int :: Int -> Int
foreign import ecall "Num:" negate_int :: Int -> Int
foreign import ecall "Num:" signum_int :: Int -> Int
foreign import ecall "Num:" integerToInt :: Integer -> Int

instance Num Int where
    (+) = add_int
    (-) = subtract_int
    (*) = multiply_int
    abs = abs_int
    negate = negate_int
    signum = signum_int
    fromInteger = integerToInt


foreign import ecall "Num:" add_double :: Double -> Double -> Double
foreign import ecall "Num:" subtract_double :: Double -> Double -> Double
foreign import ecall "Num:" multiply_double :: Double -> Double -> Double
foreign import ecall "Num:" abs_double :: Double -> Double
foreign import ecall "Num:" negate_double :: Double -> Double
foreign import ecall "Num:" signum_double :: Double -> Double
--foreign import ecall "Num:" intToDouble :: Int -> Double -- used by Enum Double
foreign import ecall "Num:" integerToDouble :: Integer -> Double

instance Num Double where
    (+) = add_double
    (-) = subtract_double
    (*) = multiply_double
    abs = abs_double
    negate = negate_double
    signum = signum_double
    fromInteger = integerToDouble

-- These may get used in other modules...
foreign import ecall "Num:" intToInteger :: Int -> Integer
foreign import ecall "Num:" intToDouble :: Int -> Double
foreign import ecall "Prelude:" doubleToInt :: Double -> Int

