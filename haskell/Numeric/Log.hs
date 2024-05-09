module Numeric.Log where

import Compiler.Floating

data Log a = Exp a

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
    recip (Exp x) = Exp $ -x
    fromRational x = Exp $ log x

instance Pow (Log a) where
    pow (Exp x) t = Exp $ x*t
    ln (Exp x) = x
    expTo x = Exp x
-}


class Floating a => Precise a where { }

foreign import bpcall "Real:" expm1 :: Double -> Double
foreign import bpcall "Real:" log1p :: Double -> Double
foreign import bpcall "Real:" log1pexp :: Double -> Double
foreign import bpcall "Real:" log1mexp :: Double -> Double

