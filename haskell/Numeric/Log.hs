module Numeric.Log where

data Log a = Log {ln :: a}

class Floating a => Precise a where { }

builtin "Real:expm1" expm1 1
builtin "Real:log1p" log1p 1
