{-# LANGUAGE NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import System.IO (print)

data A = A
data B = B

data family F a

data instance F A = MkFA { getF :: Int, setF :: Int }
data instance F B = MkFB { getF :: Int, setF :: Int }

main = print (getF (MkFA 1 2))
