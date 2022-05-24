{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where

infixl 7 *
infixl 6 +, -

(+),(-),(*) :: a -> a -> a

builtin + 2 "add" "Prelude"
builtin - 2 "subtract" "Prelude"
builtin * 2 "multiply" "Prelude"

negate, abs :: a -> a
builtin negate 1 "negate" "Prelude"
builtin abs    1 "abs"    "Prelude"

class Num a

data Integer
