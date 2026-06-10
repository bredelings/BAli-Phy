{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

data Tag = Tag

class C a where
  data N a

instance C Tag where
  newtype N Tag = BadN Tag Tag

main = IO (\s -> (s, ()))
