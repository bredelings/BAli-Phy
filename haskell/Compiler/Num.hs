{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

foreign import bpcall "Prelude:add"      (+) :: a -> a -> a
foreign import bpcall "Prelude:subtract" (-) :: a -> a -> a
foreign import bpcall "Prelude:multiply" (*) :: a -> a -> a

foreign import bpcall "Prelude:negate" negate :: a -> a
foreign import bpcall "Prelude:abs"    abs    :: a -> a

-- fromInteger x :: Double
foreign import bpcall "Prelude:intToDouble" intToDouble :: Int -> Double

class Num a

data Integer
