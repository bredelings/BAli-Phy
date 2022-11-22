{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Integral where

import Compiler.Enum
import Compiler.Real
import Compiler.Num (intToInteger,fromInteger)

infixl 7 `quot`, `rem`, `div`, `mod`

class (Real a, Enum a) => Integral a  where
    quot :: a -> a -> a
    rem  :: a -> a -> a
    div  :: a -> a -> a
    mod  :: a -> a -> a
    quotRem :: a -> a -> (a,a)
    divMod  :: a -> a -> (a,a)
    toInteger :: a -> Integer

    quotRem x y = (x `quot` y, x `rem` y)
    divMod x y = (x `div` y, x `mod` y)

foreign import bpcall "Prelude:"  div_int :: Int -> Int -> Int
foreign import bpcall "Prelude:"  mod_int :: Int -> Int -> Int
foreign import bpcall "Prelude:"  quot_int :: Int -> Int -> Int
foreign import bpcall "Prelude:"  rem_int :: Int -> Int -> Int

foreign import bpcall "Prelude:"  div_integer :: Integer -> Integer -> Integer
foreign import bpcall "Prelude:"  mod_integer :: Integer -> Integer -> Integer
foreign import bpcall "Prelude:"  quot_integer :: Integer -> Integer -> Integer
foreign import bpcall "Prelude:"  rem_integer :: Integer -> Integer -> Integer

instance Integral Int where
    quot = quot_int
    rem = rem_int
    div = div_int
    mod = mod_int
    toInteger = intToInteger

instance Integral Integer where
    quot = quot_integer
    rem = rem_integer
    div = div_integer
    mod = mod_integer
    toInteger x = x
    
    
fromIntegral x = fromInteger (toInteger x)
