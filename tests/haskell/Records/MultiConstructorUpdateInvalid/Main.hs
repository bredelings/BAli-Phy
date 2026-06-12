{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Pair = HasX { x :: Int } | HasY { y :: Int }

bad :: Pair -> Pair
bad p = p { x = 1, y = 2 }

main = print (1 :: Int)
