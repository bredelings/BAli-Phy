{-# LANGUAGE NoImplicitPrelude #-}
module Main where

type role NominalBox nominal

data NominalBox a = NominalBox a

class Bad a where
  bad :: NominalBox a -> NominalBox a

instance Bad Int where
  bad x = x

newtype Age = Age Int deriving newtype Bad

main = bad
