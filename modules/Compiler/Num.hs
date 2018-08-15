{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Num where
{
infixl 6 +, -;
infixl 7 *, /;
builtin + 2 "add" "Prelude";
builtin - 2 "subtract" "Prelude";
builtin / 2 "divide" "Prelude";
builtin * 2 "multiply" "Prelude";
}
