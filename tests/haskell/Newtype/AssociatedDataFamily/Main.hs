{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Eq
import System.IO (print)

data Tag = Tag deriving Eq

class C a where
  data N a

instance C Tag where
  newtype N Tag = MkN { unN :: Tag } deriving Eq

ok = unN (MkN Tag) == Tag

main = print (if ok then (1 :: Int) else 0)
