{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data A = A
data B = B

data family F a

data instance F A = MkFA { getA :: Int, setA :: Int }
data instance F B = MkFB { getB :: Int, setB :: Int }

main = print (getA (MkFA 1 2), setB (MkFB 3 4))
