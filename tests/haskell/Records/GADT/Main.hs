{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box a where
    Box :: { value :: a, tag :: Int } -> Box a

readBox :: Box Int -> (Int, Int)
readBox (Box { value = x, tag = n }) = (x, n)

main = print (readBox (Box { tag = 7, value = 3 }))
