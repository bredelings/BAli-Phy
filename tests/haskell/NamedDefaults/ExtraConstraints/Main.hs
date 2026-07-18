{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

class C a
instance C Char
instance C Int

default C (Char)

choose :: C a => a -> a
choose x = x

main = putStrLn $ show (choose 65)
