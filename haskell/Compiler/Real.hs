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

foreign import bpcall "Prelude:divide" (/) 2
foreign import bpcall "Prelude:div" div 2
foreign import bpcall "Prelude:mod" mod 2
foreign import bpcall "Prelude:quot" quot 2
foreign import bpcall "Prelude:rem" rem 2

class Fractional a => Floating a where { }
-- pi :: a
-- exp, sqrt, log :: a -> a
-- (**), logBase :: a -> a -> a
-- sin, tan, cos :: a -> a
-- asin, atan, acos :: a -> a
-- sinh, tanh, cosh :: a -> a
-- asinh, atanh, acosh :: a -> a

pi = 3.14159265358979323846
foreign import bpcall "Real:exp" exp 1
foreign import bpcall "Real:sqrt" sqrt 1
foreign import bpcall "Real:log" log 1
foreign import bpcall "Real:pow" (**) 2
foreign import bpcall "Real:logBase" logBase 2
foreign import bpcall "Real:sin" sin 1
foreign import bpcall "Real:tan" tan 1
foreign import bpcall "Real:cos" cos 1
foreign import bpcall "Real:asin" asin 1
foreign import bpcall "Real:atan" atan 1
foreign import bpcall "Real:acos" acos 1
foreign import bpcall "Real:sinh" sinh 1
foreign import bpcall "Real:tanh" tanh 1
foreign import bpcall "Real:cosh" cosh 1
foreign import bpcall "Real:asinh" asinh 1
foreign import bpcall "Real:atanh" atanh 1
foreign import bpcall "Real:acosh" acosh 1

class (Real a, Fractional a) => RealFrac a where
    properFraction :: (Integral b) => a -> (b,a)
--    truncate, round  :: (Integral b) => a -> b
--    ceiling, floor   :: (Integral b) => a -> b

foreign import bpcall "Prelude:truncate" truncate 1
foreign import bpcall "Prelude:ceiling" ceiling 1
foreign import bpcall "Prelude:floor" floor 1
foreign import bpcall "Prelude:round" round 1

foreign import bpcall "Prelude:doubleToInt" doubleToInt 1

foreign import bpcall "Prelude:expToLogDouble" expToLogDouble 1
foreign import bpcall "Prelude:doubleToLogDouble" doubleToLogDouble 1
foreign import bpcall "Prelude:intToDouble" intToDouble 1

-- We need == to use GHC's code directly
x0 ^ y0 | y0 < 0 = error("Negative exponent")
x ^ 1 = x
x ^ n = x*(x^(n-1))

recip y = 1.0/y -- should be 1/y

x ^^ n = if n >= 0 then x^n else recip (x^(negate n))
