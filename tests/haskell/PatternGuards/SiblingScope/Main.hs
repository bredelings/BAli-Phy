{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Bool
import Data.Function (($))
import Data.Maybe
import System.IO (putStrLn)
import Text.Show (show)

result = f Nothing
  where
    y = 7
    f x | Just y <- x = y
        | otherwise = y

main = putStrLn $ show result
