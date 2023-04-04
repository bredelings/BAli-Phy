{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Floating where

import Compiler.Error
import Compiler.Num -- for LogDouble
import Compiler.Fractional
import Compiler.Integral -- for quot, rem
import Data.Function -- for (.)
import Data.Ord

infixl 8 **

class Fractional a => Floating a where
    pi :: a
    exp, sqrt, log :: a -> a
    (**), logBase :: a -> a -> a
    sin, tan, cos :: a -> a
    asin, atan, acos :: a -> a
    sinh, tanh, cosh :: a -> a
    asinh, atanh, acosh :: a -> a

foreign import bpcall "Real:exp" exp_double :: Double -> Double
foreign import bpcall "Real:sqrt" sqrt_a :: a -> a
foreign import bpcall "Real:log" log_a :: a -> Double
foreign import bpcall "Real:pow" pow_a :: a -> Double -> a
foreign import bpcall "Real:logBase" logBase_a :: a -> Double -> a
foreign import bpcall "Real:sin" sin_double :: Double -> Double
foreign import bpcall "Real:tan" tan_double :: Double -> Double
foreign import bpcall "Real:cos" cos_double :: Double -> Double
foreign import bpcall "Real:asin" asin_double :: Double -> Double
foreign import bpcall "Real:atan" atan_double :: Double -> Double
foreign import bpcall "Real:acos" acos_double :: Double -> Double
foreign import bpcall "Real:sinh" sinh_double :: Double -> Double
foreign import bpcall "Real:tanh" tanh_double :: Double -> Double
foreign import bpcall "Real:cosh" cosh_double :: Double -> Double
foreign import bpcall "Real:asinh" asinh_double :: Double -> Double
foreign import bpcall "Real:atanh" atanh_double :: Double -> Double
foreign import bpcall "Real:acosh" acosh_double :: Double -> Double

instance Floating Double where
    pi = 3.14159265358979323846
    exp = exp_double
    sqrt = sqrt_a
    log = log_a
    (**) = pow_a
    logBase = logBase_a

    sin = sin_double
    tan = tan_double
    cos = cos_double
    asin = asin_double
    atan = atan_double
    acos = acos_double
    sinh = sinh_double
    tanh = tanh_double
    cosh = cosh_double
    asinh = asinh_double
    atanh = atanh_double
    acosh = acosh_double

foreign import bpcall "Prelude:expToLogDouble" expToLogDouble :: Double -> LogDouble

infixr 8 `pow`

class Fractional a => Pow a where
    pow :: a -> Double -> a
    ln  :: a -> Double
    expTo :: Double -> a

instance Pow Double where
    pow   = pow_a
    ln    = log_a
    expTo = exp_double

instance Pow LogDouble where
    pow   = pow_a
    ln    = log_a
    expTo = expToLogDouble
