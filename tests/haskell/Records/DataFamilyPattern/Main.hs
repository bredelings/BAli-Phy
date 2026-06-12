{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Tag = Tag

data family F a

data instance F Tag = MkF { getF :: Int, setF :: Int }

setBoth f a b = f { getF = a, setF = b }

readF (MkF { setF = b, getF = a }) = (a, b)

main = print (readF (setBoth (MkF 1 2) 7 8))
