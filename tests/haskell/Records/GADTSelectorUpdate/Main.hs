{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data G a b where
    MkG :: { bar :: a, tag :: Int } -> G a Int

setBar :: G Int b -> G Int b
setBar g = g { bar = bar g + 1 }

readG :: G Int Int -> (Int, Int)
readG g = (bar g, tag g)

main = print (readG (setBar (MkG { bar = 4, tag = 9 })))
