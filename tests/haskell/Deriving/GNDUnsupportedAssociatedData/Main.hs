{-# LANGUAGE NoImplicitPrelude #-}
module Main where

class HasData a where
  data Assoc a
  makeAssoc :: a -> Assoc a

instance HasData Int where
  data Assoc Int = AssocInt
  makeAssoc x = AssocInt

newtype Age = Age Int deriving newtype HasData

main = makeAssoc
