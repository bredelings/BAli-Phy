module Numeric.Log where

import Compiler.Floating

data Log a = Log a

{-
This requires (at least) log to be in class Floating

instance Num (Log a) where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    negate = error "Negation not allowed"
    abs = id
    signum _ = (Log 0)
    fromInteger = undefined

instance Fractional (Log a) where
    recip (Log x) = Log $ -x
    fromRational x = Log $ log x

instance Pow (Log a) where
    pow (Log x) t = Log $ x*t
    ln (Log x) = x
    expTo x = Log x
-}


class Floating a => Precise a where { }

foreign import bpcall "Real:" expm1 :: Double -> Double
foreign import bpcall "Real:" log1p :: Double -> Double
