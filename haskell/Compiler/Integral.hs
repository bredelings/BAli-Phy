{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Integral where

import Compiler.Error
import Compiler.Enum
import Compiler.Real
import Compiler.Num -- (intToInteger,fromInteger)
import Data.Eq
import Data.Function -- for (.)
import Data.Ord      -- for (<)

infixl 7 `quot`, `rem`, `div`, `mod`
infixl 8 ^

class (Real a, Enum a) => Integral a  where
    quot :: a -> a -> a
    rem  :: a -> a -> a
    div  :: a -> a -> a
    mod  :: a -> a -> a
    quotRem :: a -> a -> (a,a)
    divMod  :: a -> a -> (a,a)
    toInteger :: a -> Integer

    div x y = d where (d,_) = divMod x y
    mod x y = m where (_,m) = divMod x y
    quot x y = q where (q,_) = quotRem x y
    rem x y = r where (_,r) = quotRem x y

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d

    quotRem x y = (x `quot` y, x `rem` y)

foreign import ecall "Prelude:"  div_int :: Int -> Int -> Int
foreign import ecall "Prelude:"  mod_int :: Int -> Int -> Int
foreign import ecall "Prelude:"  quot_int :: Int -> Int -> Int
foreign import ecall "Prelude:"  rem_int :: Int -> Int -> Int

foreign import ecall "Prelude:"  div_integer :: Integer -> Integer -> Integer
foreign import ecall "Prelude:"  mod_integer :: Integer -> Integer -> Integer
foreign import ecall "Prelude:"  quot_integer :: Integer -> Integer -> Integer
foreign import ecall "Prelude:"  rem_integer :: Integer -> Integer -> Integer

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

even n = n `rem` 2 == 0
odd = not . even

(^) :: (Num a, Integral b) => a -> b -> a
x ^ y | y < 0   = error "Negative exponent"
      | y == 0  = 1
      | y == 1  = x
      | even y   = (x*x) ^ (y `quot` 2)        -- y >= 2
      | otherwise = x * ((x*x) ^ (y `quot` 2)) -- y >= 3
