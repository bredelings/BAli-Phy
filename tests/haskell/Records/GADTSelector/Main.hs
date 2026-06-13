{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data G a b where
    MkG :: { bar :: a, tag :: Int } -> G a Int

readG :: G Int Int -> (Int, Int)
readG g = (bar g, tag g)

main = print (readG (MkG { bar = 4, tag = 9 }))
