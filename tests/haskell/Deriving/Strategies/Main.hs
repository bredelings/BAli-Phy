{-# LANGUAGE NoImplicitPrelude #-}
module Main where

class Empty a where
  emptyMethod :: a -> a

data T = T deriving anyclass Empty

newtype U = U T deriving Empty via T

main = emptyMethod T
