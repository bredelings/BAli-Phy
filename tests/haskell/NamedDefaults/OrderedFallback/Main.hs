{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Bool
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

class C a
instance C Bool
instance C Int

default C (Bool, Int)

choose :: C a => a -> a
choose x = x

main = putStrLn $ show (choose 65)
