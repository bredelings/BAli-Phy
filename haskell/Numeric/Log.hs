module Numeric.Log where

import Compiler.Floating

data Log a = Exp a

instance RealFloat a => Num (Log a) where
    Exp x + Exp y = Exp (x + log1p(exp(y-x)))
    Exp x - Exp y = Exp (x - log1p(exp(y-x)))
    Exp x * Exp y = Exp (x + y)
    negate = error "Negation not allowed"
    abs = id
    signum _ = Exp 0
    fromInteger x = Exp (log (fromInteger x))

instance RealFloat a => Fractional (Log a) where
    Exp x / Exp y = Exp (x - y)
    recip (Exp x) = Exp $ -x

instance Pow (Log Double) where
    pow (Exp x) t = Exp $ x*t
    ln (Exp x) = x
    expTo x = Exp x


