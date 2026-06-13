{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data G a b where
    MkG :: { bar :: a, tag :: Int } -> G a Int

data T a where
    MkT :: { item :: a } -> T [a]

readG :: G Int Int -> (Int, Int)
readG g = (bar g, tag g)

readT :: T [Int] -> Int
readT t = item t

main = print (readG (MkG { bar = 4, tag = 9 }), readT (MkT { item = 3 }))
