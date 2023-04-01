{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Num
import Compiler.Error
import Data.Ord
import Compiler.Ratio

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational


instance Real Int where
    toRational n = Ratio (intToInteger n) 1

instance Real Integer where
    toRational n = Ratio n 1

instance Real Double where
    toRational x = error "undefined"

foreign import bpcall "Prelude:" equals_log_double :: LogDouble -> LogDouble -> Bool

instance Eq LogDouble where
    (==) = equals_log_double

foreign import bpcall "Prelude:" lessthan_log_double :: LogDouble -> LogDouble -> Bool

instance Ord LogDouble where
    (<) = lessthan_log_double

instance Real LogDouble where
    toRational x = error "undefined"
