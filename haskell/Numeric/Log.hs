module Numeric.Log where

data Log a = Log {ln :: a}

class Floating a => Precise a where { }

foreign import bpcall "Real:expm1" expm1 1
foreign import bpcall "Real:log1p" log1p 1
