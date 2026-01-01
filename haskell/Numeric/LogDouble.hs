{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.LogDouble where

import Data.Bool
import Data.Eq
import Data.Ord
import Text.Show
import Data.Function
import Compiler.Base (error)
import Compiler.Num
import Compiler.Integral
import Compiler.Fractional
import Compiler.Floating
import Compiler.Real
import Data.Floating.Types

data LogDouble

foreign import ecall "Num:" add_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import ecall "Num:" subtract_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import ecall "Num:" multiply_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import ecall "Num:" signum_logdouble :: LogDouble -> LogDouble
foreign import ecall "Num:" integerToLogDouble :: Integer -> LogDouble
foreign import bpcall "Prelude:" lessthan_log_double :: LogDouble -> LogDouble -> Bool
foreign import bpcall "Prelude:" divide_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Prelude:" recip_logdouble :: LogDouble -> LogDouble
foreign import bpcall "Prelude:" doubleToLogDouble :: Double -> LogDouble
foreign import bpcall "Prelude:" equals_log_double :: LogDouble -> LogDouble -> Bool
foreign import bpcall "Prelude:" expToLogDouble :: Double -> LogDouble


instance Eq LogDouble where
    (==) = equals_log_double

instance Ord LogDouble where
    (<) = lessthan_log_double

instance Num LogDouble where
    (+) = add_logdouble
    (-) = subtract_logdouble
    (*) = multiply_logdouble
    abs x = x
    negate = error "negate LogDouble"
    signum = signum_logdouble
    fromInteger = integerToLogDouble

instance Fractional LogDouble where
    (/) = divide_logdouble
    recip = recip_logdouble

instance Real LogDouble where
    toRational x = error "undefined"

instance Pow LogDouble where
    pow   = pow_a
    ln    = log_a
    expTo = expToLogDouble

instance Show LogDouble where
    show x = show (toFloating x :: Double)

instance FloatConvert LogDouble Double where
    toFloating x = exp $ ln x

instance FloatConvert Double LogDouble where
    toFloating x = expTo $ log x

instance FloatConvert Int LogDouble where
    toFloating i = toFloating (toFloating i :: Double)

instance FloatConvert Integer LogDouble where
    toFloating i = toFloating (toFloating i :: Double)

class ToLogDouble a where
    toLogDouble :: a -> LogDouble

instance ToLogDouble Double where
    toLogDouble x = doubleToLogDouble x

instance ToLogDouble LogDouble where
    toLogDouble x = x

instance ToLogDouble Int where
    toLogDouble x = doubleToLogDouble $ fromIntegral x

instance ToLogDouble Integer where
    toLogDouble x = doubleToLogDouble $ fromIntegral x
