{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO (print)

class Tag a where
  tag :: a -> Int
  tag x = 42

data T = T deriving anyclass Tag

main = print (tag T)
