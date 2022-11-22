{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.RealFrac where

import Compiler.Real
import Compiler.Fractional
import Compiler.Integral

class (Real a, Fractional a) => RealFrac a where
    properFraction :: (Integral b) => a -> (b,a)
--    truncate, round  :: (Integral b) => a -> b
--    ceiling, floor   :: (Integral b) => a -> b

foreign import bpcall "Prelude:truncate" truncate :: Double -> Int
foreign import bpcall "Prelude:ceiling" ceiling :: Double -> Int
foreign import bpcall "Prelude:floor" floor :: Double -> Int
foreign import bpcall "Prelude:round" round :: Double -> Int
