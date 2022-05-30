{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

foreign import bpcall "Prelude:add"      (+) :: a -> a -> a
foreign import bpcall "Prelude:subtract" (-) :: a -> a -> a
foreign import bpcall "Prelude:multiply" (*) :: a -> a -> a

foreign import bpcall "Prelude:negate" negate :: a -> a
foreign import bpcall "Prelude:abs"    abs    :: a -> a
-- foreign import bpcall "Prelude:signum"    signum    :: a -> a

fromInteger :: Int -> Int
fromInteger x = x

fromRational :: Double -> Double
fromRational x = x

-- for fromInteger x :: Double
foreign import bpcall "Prelude:intToDouble" intToDouble :: Int -> Double

class Num a

data Integer

