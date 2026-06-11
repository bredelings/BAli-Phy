{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Tag = Tag

data family F a

data instance F Tag = MkF { getF :: Int, setF :: Int }

swap f = f { getF = setF f, setF = getF f }

main = print (getF (swap (MkF { getF = 1, setF = 2 })))
