module Numeric.Log where

import Compiler.Floating

data Log a = Exp a

instance RealFloat a => Num (Log a) where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    negate = error "Negation not allowed"
    abs = id
    signum _ = Exp 0
    fromInteger = undefined

instance RealFloat a => Fractional (Log a) where
    (Exp x) / (Exp y) = Exp (x - y)
    recip (Exp x) = Exp $ -x

{-
instance RealFloat a => Pow (Log a) where
    pow (Exp x) t = Exp $ x*t
    ln (Exp x) = x
    expTo x = Exp x
-}



