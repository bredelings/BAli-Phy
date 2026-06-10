{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Eq
import System.IO (print)

class Tag a where
  tag :: a -> Int

instance Tag Int where
  tag x = x

newtype Age = Age Int deriving Tag via Int

main = print (if tag (Age 42) == 42 then (1 :: Int) else 0)
