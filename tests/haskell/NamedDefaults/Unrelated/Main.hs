{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Main where

import A (C)
import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

default C (Char)

main = putStrLn $ show 65
