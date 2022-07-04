{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Base
import Compiler.Error
import Compiler.Num
import Compiler.Enum -- for class Enum
import Data.Ord      -- for <
import Data.Function -- for .

infixl 8 ^, ^^, **
infixl 7 /, `quot`, `rem`, `div`, `mod`

class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
-- fromRational :: Rational -> a
    fromRational :: Double -> a

foreign import bpcall "Prelude:" divide_double :: Double -> Double -> Double
foreign import bpcall "Prelude:" recip_double :: Double -> Double

instance Fractional Double where
    (/) = divide_double
    recip = recip_double
    fromRational x = x


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

foreign import bpcall "Prelude:div" div :: a -> a -> a
foreign import bpcall "Prelude:mod" mod :: a -> a -> a
foreign import bpcall "Prelude:quot" quot :: a -> a -> a
foreign import bpcall "Prelude:rem" rem :: a -> a -> a

class Fractional a => Floating a where { }
-- pi :: a
-- exp, sqrt, log :: a -> a
-- (**), logBase :: a -> a -> a
-- sin, tan, cos :: a -> a
-- asin, atan, acos :: a -> a
-- sinh, tanh, cosh :: a -> a
-- asinh, atanh, acosh :: a -> a

pi = 3.14159265358979323846
foreign import bpcall "Real:exp" exp :: Double -> Double
foreign import bpcall "Real:sqrt" sqrt :: Double -> Double
foreign import bpcall "Real:log" log :: Double -> Double
foreign import bpcall "Real:pow" (**) :: a -> a -> a
foreign import bpcall "Real:logBase" logBase :: Double -> Double -> Double
foreign import bpcall "Real:sin" sin :: Double -> Double
foreign import bpcall "Real:tan" tan :: Double -> Double
foreign import bpcall "Real:cos" cos :: Double -> Double
foreign import bpcall "Real:asin" asin :: Double -> Double
foreign import bpcall "Real:atan" atan :: Double -> Double
foreign import bpcall "Real:acos" acos :: Double -> Double
foreign import bpcall "Real:sinh" sinh :: Double -> Double
foreign import bpcall "Real:tanh" tanh :: Double -> Double
foreign import bpcall "Real:cosh" cosh :: Double -> Double
foreign import bpcall "Real:asinh" asinh :: Double -> Double
foreign import bpcall "Real:atanh" atanh :: Double -> Double
foreign import bpcall "Real:acosh" acosh :: Double -> Double

class (Real a, Fractional a) => RealFrac a where
    properFraction :: (Integral b) => a -> (b,a)
--    truncate, round  :: (Integral b) => a -> b
--    ceiling, floor   :: (Integral b) => a -> b

foreign import bpcall "Prelude:truncate" truncate :: Double -> Int
foreign import bpcall "Prelude:ceiling" ceiling :: Double -> Int
foreign import bpcall "Prelude:floor" floor :: Double -> Int
foreign import bpcall "Prelude:round" round :: Double -> Int

foreign import bpcall "Prelude:" doubleToInt :: Double -> Int

data LogDouble

foreign import bpcall "Num:" add_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" subtract_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" multiply_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" signum_logdouble :: LogDouble -> LogDouble
foreign import bpcall "Num:" intToLogDouble :: Int -> LogDouble

instance Num LogDouble where
    (+) = add_logdouble
    (-) = subtract_logdouble
    (*) = multiply_logdouble
    abs x = x
    negate = error "negate LogDouble"
    signum = signum_logdouble
    fromInteger = intToLogDouble

foreign import bpcall "Prelude:" divide_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Prelude:" recip_logdouble :: LogDouble -> LogDouble

instance Fractional LogDouble where
    (/) = divide_logdouble
    recip = recip_logdouble
    fromRational = doubleToLogDouble

foreign import bpcall "Prelude:expToLogDouble" expToLogDouble :: Double -> LogDouble
foreign import bpcall "Prelude:doubleToLogDouble" doubleToLogDouble :: Double -> LogDouble

-- We need == to use GHC's code directly
(^) :: Num a => a -> Int -> a
x0 ^ y0 | y0 < 0 = error "Negative exponent"
x ^ 1 = x
x ^ n = x*(x^(n-1))

x ^^ n = if n >= 0 then x^n else recip (x^(negate n))

infixr 8 `pow`
foreign import bpcall "Real:pow" pow :: LogDouble -> Double -> LogDouble
