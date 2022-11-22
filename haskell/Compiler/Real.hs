{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Num
import Data.Ord
import Data.Ratio

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational



