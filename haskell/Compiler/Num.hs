{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

(+),(-),(*) :: a -> a -> a

foreign import bpcall "Prelude:add" (+) 2
foreign import bpcall "Prelude:subtract" (-) 2
foreign import bpcall "Prelude:multiply" (*) 2

negate, abs :: a -> a
foreign import bpcall "Prelude:negate" negate 1
foreign import bpcall "Prelude:abs" abs    1

class Num a

data Integer
