{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Semigroup where

infixr 6 <>

class Semigroup a where
    (<>) :: a -> a -> a 
