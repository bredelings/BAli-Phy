{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Main where

import Compiler.Base (String)
import Compiler.Num
import System.IO (print)
import Text.Show (Show, show)

g :: Int -> forall a. Show a => a -> String
g = \_ x -> show x

main = print (g 0 7)
