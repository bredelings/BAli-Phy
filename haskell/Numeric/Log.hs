{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Log where

import Compiler.Base (error)
import Compiler.Classes ()
import Compiler.FFI.Runtime (RuntimeValue)
import Compiler.Floating
import Compiler.Fractional
import Compiler.Integral
import Compiler.Num
import Compiler.Real
import Data.Eq
import Data.Floating.Types
import Data.Function
import Data.Ord
import Text.Show

newtype Log a = Exp a

instance RuntimeValue (Log Double)

foreign import ecall "Num:" add_logdouble :: Log Double -> Log Double -> Log Double
foreign import ecall "Num:" subtract_logdouble :: Log Double -> Log Double -> Log Double
foreign import ecall "Num:" multiply_logdouble :: Log Double -> Log Double -> Log Double
foreign import ecall "Num:" signum_logdouble :: Log Double -> Log Double
foreign import ecall "Num:" integerToLogDouble :: Integer -> Log Double
foreign import ecall "Prelude:" lessthan_log_double :: Log Double -> Log Double -> Bool
foreign import ecall "Prelude:" divide_logdouble :: Log Double -> Log Double -> Log Double
foreign import ecall "Prelude:" recip_logdouble :: Log Double -> Log Double
foreign import ecall "Prelude:" doubleToLogDouble :: Double -> Log Double
foreign import ecall "Prelude:" equals_log_double :: Log Double -> Log Double -> Bool

instance Eq (Log Double) where
    (==) = equals_log_double

instance Ord (Log Double) where
    (<) = lessthan_log_double

instance Num (Log Double) where
    (+) = add_logdouble
    (-) = subtract_logdouble
    (*) = multiply_logdouble
    abs x = x
    negate = error "negate (Log Double)"
    signum = signum_logdouble
    fromInteger = integerToLogDouble

instance Fractional (Log Double) where
    (/) = divide_logdouble
    recip = recip_logdouble

instance Real (Log Double) where
    toRational _ = error "undefined"

instance Pow (Log Double) where
    pow (Exp x) t = Exp (x * t)
    ln (Exp x) = x
    expTo = Exp

instance Show (Log Double) where
    show x = show (toFloating x :: Double)

instance FloatConvert (Log Double) Double where
    toFloating (Exp x) = exp x

instance FloatConvert Double (Log Double) where
    toFloating = doubleToLogDouble

instance FloatConvert Int (Log Double) where
    toFloating i = toFloating (toFloating i :: Double)

instance FloatConvert Integer (Log Double) where
    toFloating i = toFloating (toFloating i :: Double)
