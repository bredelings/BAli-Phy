{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Base
import Compiler.Num
import Compiler.Enum -- for class Enum
import Data.Ord      -- for <
import Data.Function -- for .

infixl 8 ^, ^^, **
infixl 7 /, `quot`, `rem`, `div`, `mod`

class Num a => Fractional a where { }
-- (/) :: a -> a -> a
-- recip :: a -> a
-- fromRational :: Rational -> a

class (Num a, Ord a) => Real a where { }
--    toRational :: a -> Rational

class (Real a, Enum a) => Integral a  where { }
-- quot :: a -> a -> a
-- rem  :: a -> a -> a
-- div  :: a -> a -> a
-- mod  :: a -> a -> a
-- quotRem :: a -> a -> (a,a)
-- divMod  :: a -> a -> (a,a)
-- toInteger :: a -> Integer

builtin (/) 2 "Prelude:divide"
builtin div 2 "Prelude:div"
builtin mod 2 "Prelude:mod"
builtin quot 2 "Prelude:quot"
builtin rem 2 "Prelude:rem"

class Fractional a => Floating a where { }
-- pi :: a
-- exp, sqrt, log :: a -> a
-- (**), logBase :: a -> a -> a
-- sin, tan, cos :: a -> a
-- asin, atan, acos :: a -> a
-- sinh, tanh, cosh :: a -> a
-- asinh, atanh, acosh :: a -> a

pi = 3.14159265358979323846
builtin exp 1 "Real:exp"
builtin sqrt 1 "Real:sqrt"
builtin log 1 "Real:log"
builtin (**) 2 "Real:pow"
builtin logBase 2 "Real:logBase"
builtin sin 1 "Real:sin"
builtin tan 1 "Real:tan"
builtin cos 1 "Real:cos"
builtin asin 1 "Real:asin"
builtin atan 1 "Real:atan"
builtin acos 1 "Real:acos"
builtin sinh 1 "Real:sinh"
builtin tanh 1 "Real:tanh"
builtin cosh 1 "Real:cosh"
builtin asinh 1 "Real:asinh"
builtin atanh 1 "Real:atanh"
builtin acosh 1 "Real:acosh"

class (Real a, Fractional a) => RealFrac a where
    properFraction :: (Integral b) => a -> (b,a)
--    truncate, round  :: (Integral b) => a -> b
--    ceiling, floor   :: (Integral b) => a -> b

builtin truncate 1 "Prelude:truncate"
builtin ceiling 1 "Prelude:ceiling"
builtin floor 1 "Prelude:floor"
builtin round 1 "Prelude:round"

builtin doubleToInt 1 "Prelude:doubleToInt"

builtin expToLogDouble 1 "Prelude:expToLogDouble"
builtin doubleToLogDouble 1 "Prelude:doubleToLogDouble"
builtin intToDouble 1 "Prelude:intToDouble"

-- We need == to use GHC's code directly
x0 ^ y0 | y0 < 0 = error("Negative exponent")
x ^ 1 = x
x ^ n = x*(x^(n-1))

recip y = 1.0/y -- should be 1/y

x ^^ n = if n >= 0 then x^n else recip (x^(negate n))
