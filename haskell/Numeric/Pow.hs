{-# LANGUAGE NoImplicitPrelude, NamedDefaults #-}
module Numeric.Pow (Pow(..), default Pow) where

import Compiler.Fractional
import qualified Compiler.Floating as Floating

infixr 8 `pow`

-- How about log1p, exmp1, log1pexp, and log1mexp?

class Fractional a => Pow a where
    ln  :: a -> Double
    expTo :: Double -> a
    lnBase :: a -> a -> Double

    sq_rt :: a -> a
    pow :: a -> Double -> a

    sq_rt x = pow x 0.5
    lnBase x y = ln x / ln y

instance Pow Double where
    ln    = Floating.log
    expTo = Floating.exp

    sq_rt  = Floating.sqrt
    pow   = (Floating.**)

default Pow (Double)
