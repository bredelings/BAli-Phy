module Numeric.Log where

data Log a = Log {ln :: a}

class Floating a => Precise a where { }

builtin expm1 1 "expm1" "Real"
builtin log1p 1 "log1p" "Real"
