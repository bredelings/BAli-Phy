{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Num
import Data.Ord
import Compiler.Ratio

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational



