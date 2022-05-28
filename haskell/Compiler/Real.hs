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

foreign import bpcall "Prelude:divide" (/) :: () -> () -> ()
foreign import bpcall "Prelude:div" div :: () -> () -> ()
foreign import bpcall "Prelude:mod" mod :: () -> () -> ()
foreign import bpcall "Prelude:quot" quot :: () -> () -> ()
foreign import bpcall "Prelude:rem" rem :: () -> () -> ()

class Fractional a => Floating a where { }
-- pi :: a
-- exp, sqrt, log :: a -> a
-- (**), logBase :: a -> a -> a
-- sin, tan, cos :: a -> a
-- asin, atan, acos :: a -> a
-- sinh, tanh, cosh :: a -> a
-- asinh, atanh, acosh :: a -> a

pi = 3.14159265358979323846
foreign import bpcall "Real:exp" exp :: () -> ()
foreign import bpcall "Real:sqrt" sqrt :: () -> ()
foreign import bpcall "Real:log" log :: () -> ()
foreign import bpcall "Real:pow" (**) :: () -> () -> ()
foreign import bpcall "Real:logBase" logBase :: () -> () -> ()
foreign import bpcall "Real:sin" sin :: () -> ()
foreign import bpcall "Real:tan" tan :: () -> ()
foreign import bpcall "Real:cos" cos :: () -> ()
foreign import bpcall "Real:asin" asin :: () -> ()
foreign import bpcall "Real:atan" atan :: () -> ()
foreign import bpcall "Real:acos" acos :: () -> ()
foreign import bpcall "Real:sinh" sinh :: () -> ()
foreign import bpcall "Real:tanh" tanh :: () -> ()
foreign import bpcall "Real:cosh" cosh :: () -> ()
foreign import bpcall "Real:asinh" asinh :: () -> ()
foreign import bpcall "Real:atanh" atanh :: () -> ()
foreign import bpcall "Real:acosh" acosh :: () -> ()

class (Real a, Fractional a) => RealFrac a where
    properFraction :: (Integral b) => a -> (b,a)
--    truncate, round  :: (Integral b) => a -> b
--    ceiling, floor   :: (Integral b) => a -> b

foreign import bpcall "Prelude:truncate" truncate :: () -> ()
foreign import bpcall "Prelude:ceiling" ceiling :: () -> ()
foreign import bpcall "Prelude:floor" floor :: () -> ()
foreign import bpcall "Prelude:round" round :: () -> ()

foreign import bpcall "Prelude:doubleToInt" doubleToInt :: () -> ()

foreign import bpcall "Prelude:expToLogDouble" expToLogDouble :: () -> ()
foreign import bpcall "Prelude:doubleToLogDouble" doubleToLogDouble :: () -> ()

-- We need == to use GHC's code directly
x0 ^ y0 | y0 < 0 = error("Negative exponent")
x ^ 1 = x
x ^ n = x*(x^(n-1))

recip y = 1.0/y -- should be 1/y

x ^^ n = if n >= 0 then x^n else recip (x^(negate n))
