{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO (print)

class Empty a where
  emptyMethod :: a -> Int
  emptyMethod x = 42

data T = T deriving anyclass Empty

newtype U = U T deriving Empty via T

main = print (emptyMethod (U T))
