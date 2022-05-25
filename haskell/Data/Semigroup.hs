{-# LANGUAGE NoImplicitPrelude #-}
module Data.Semigroup where

infixr 6 <>

class Semigroup a where
    (<>) :: a -> a -> a
