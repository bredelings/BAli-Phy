{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

class A a
class B a
instance A Int
instance A Char
instance B Int
instance B Char

default A (Int)
default B (Char)

choose :: (A a, B a) => a -> a
choose x = x

main = putStrLn $ show (choose 65)
