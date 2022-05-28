{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

(+),(-),(*) :: a -> a -> a

foreign import bpcall "Prelude:add" (+) :: () -> () -> ()
foreign import bpcall "Prelude:subtract" (-) :: () -> () -> ()
foreign import bpcall "Prelude:multiply" (*) :: () -> () -> ()

negate, abs :: a -> a
foreign import bpcall "Prelude:negate" negate :: () -> ()
foreign import bpcall "Prelude:abs" abs    :: () -> ()

class Num a

data Integer
