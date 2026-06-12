{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Tag = Tag

data family F a

data instance F Tag = MkF { left :: Int, right :: Int, extra :: Int }

readF (MkF { right = b, .. }) = (left, b, extra)

main = print (readF (MkF 4 5 6))
