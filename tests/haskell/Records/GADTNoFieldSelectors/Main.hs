{-# LANGUAGE GADTs, NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box a where
    Box :: { value :: a, tag :: Int } -> Box a

readBox (Box { value = v, tag = n }) = (v, n)

main = print (readBox (Box { value = (3 :: Int), tag = 4 }))
