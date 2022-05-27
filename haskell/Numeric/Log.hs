module Numeric.Log where

data Log a = Log {ln :: a}

class Floating a => Precise a where { }

builtin expm1 1 "Real:expm1"
builtin log1p 1 "Real:log1p"
