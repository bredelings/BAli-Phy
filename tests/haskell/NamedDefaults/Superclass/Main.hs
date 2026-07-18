{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Integral
import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

choose :: Integral a => a -> a
choose x = x

main = putStrLn $ show (choose 2)
