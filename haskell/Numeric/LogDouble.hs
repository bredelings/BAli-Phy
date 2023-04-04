module Numeric.LogDouble where

import Compiler.Floating
import Compiler.Real

data LogDouble

foreign import bpcall "Num:" add_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" subtract_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" multiply_logdouble :: LogDouble -> LogDouble -> LogDouble
foreign import bpcall "Num:" signum_logdouble :: LogDouble -> LogDouble
foreign import bpcall "Num:" integerToLogDouble :: Integer -> LogDouble
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
    fromRational = doubleToLogDouble

instance Real LogDouble where
    toRational x = error "undefined"

instance Pow LogDouble where
    pow   = pow_a
    ln    = log_a
    expTo = expToLogDouble

instance Show LogDouble where
    show x = show $ exp $ ln x

            
