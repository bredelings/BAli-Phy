{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Num
import Data.Ord
import Compiler.Ratio

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational


instance Real Int where
    toRational n = Ratio (intToInteger n) 1

instance Real Integer where
    toRational n = Ratio n 1
