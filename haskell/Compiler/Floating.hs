{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Floating where

import Compiler.Error
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
    log1p, expm1, log1pexp, log1mexp :: a -> a

foreign import ecall "Real:exp" exp_double :: Double -> Double
foreign import ecall "Real:sqrt" sqrt_a :: a -> a
foreign import ecall "Real:log" log_a :: a -> Double
foreign import ecall "Real:pow" pow_a :: a -> Double -> a
foreign import ecall "Real:logBase" logBase_a :: a -> Double -> a
foreign import ecall "Real:sin" sin_double :: Double -> Double
foreign import ecall "Real:tan" tan_double :: Double -> Double
foreign import ecall "Real:cos" cos_double :: Double -> Double
foreign import ecall "Real:asin" asin_double :: Double -> Double
foreign import ecall "Real:atan" atan_double :: Double -> Double
foreign import ecall "Real:acos" acos_double :: Double -> Double
foreign import ecall "Real:sinh" sinh_double :: Double -> Double
foreign import ecall "Real:tanh" tanh_double :: Double -> Double
foreign import ecall "Real:cosh" cosh_double :: Double -> Double
foreign import ecall "Real:asinh" asinh_double :: Double -> Double
foreign import ecall "Real:atanh" atanh_double :: Double -> Double
foreign import ecall "Real:acosh" acosh_double :: Double -> Double
foreign import ecall "Real:expm1" expm1_double :: Double -> Double
foreign import ecall "Real:log1p" log1p_double :: Double -> Double
foreign import ecall "Real:log1pexp" log1pexp_double :: Double -> Double
foreign import ecall "Real:log1mexp" log1mexp_double :: Double -> Double

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
    expm1 = expm1_double
    log1p = log1p_double
    log1pexp = log1pexp_double
    log1mexp = log1mexp_double

infixr 8 `pow`

-- How about log1p, exmp1, log1pexp, and log1mexp?

class Fractional a => Pow a where
    ln  :: a -> Double
    expTo :: Double -> a
    lnBase :: a -> a -> Double

    sq_rt :: a -> a
    pow :: a -> Double -> a

    sq_rt x = pow x 0.5
    lnBase x y = ln x / ln y

instance Pow Double where
    ln    = log_a
    expTo = exp_double

    sq_rt  = sqrt_a
    pow   = pow_a

