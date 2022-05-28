{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

(+),(-),(*) :: a -> a -> a

builtin "Prelude:add" (+) 2
builtin "Prelude:subtract" (-) 2
builtin "Prelude:multiply" (*) 2

negate, abs :: a -> a
builtin "Prelude:negate" negate 1
builtin "Prelude:abs" abs    1

class Num a

data Integer
