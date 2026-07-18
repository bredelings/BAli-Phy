{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Compiler.Num
import Data.Function (($))
import Data.String (IsString(..))
import System.IO (putStrLn)
import Text.Show (show)

instance IsString Int where
    fromString _ = 7

default (Int)

main = putStrLn $ show "ignored"
