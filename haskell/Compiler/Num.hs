{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

(+),(-),(*) :: a -> a -> a

builtin (+) 2 "Prelude:add"
builtin (-) 2 "Prelude:subtract"
builtin (*) 2 "Prelude:multiply"

negate, abs :: a -> a
builtin negate 1 "Prelude:negate"
builtin abs    1 "Prelude:abs"

class Num a

data Integer
