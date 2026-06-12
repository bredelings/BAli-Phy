{-# LANGUAGE GADTs, NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box a where
    Box :: { value :: a, tag :: Int, extra :: Int } -> Box a

readBox :: Box Int -> (Int, Int, Int)
readBox (Box { tag = n, .. }) = (value, n, extra)

main = print (readBox (Box { value = 7, tag = 8, extra = 9 }))
