{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ratio where

import Compiler.Integral
import Compiler.Ratio
import Compiler.RealFrac

infixl 7 %
(%) :: Integral a => a -> a -> Ratio a
x % y = Ratio x y

-- approxRational :: RealFrac a => a -> a -> Rational
