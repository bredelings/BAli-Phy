{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Main where

import Compiler.Base (String)
import Compiler.Num
import System.IO (print)
import Text.Show (Show, show)

f :: Int -> forall a. Show a => a -> String
f _ x = show x

main = print (f 0 7)
