{-# LANGUAGE NoImplicitPrelude #-}
module Main where

-- The parameter substitution for Pair b Int is
--
--     a |-> b, b |-> Int
--
-- and must be applied simultaneously.  Recursively rewriting the replacement
-- for a would incorrectly produce (Int, Int), or loop when names collide.
type Pair a b = (a, b)

preserveCallerVariable :: Pair b Int -> (b, Int)
preserveCallerVariable x = x

type Identity a = a

preserveSelfNamedVariable :: Identity a -> a
preserveSelfNamedVariable x = x
