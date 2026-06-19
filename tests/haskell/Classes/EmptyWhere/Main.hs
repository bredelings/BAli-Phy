{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

class LayoutMarker a where

instance LayoutMarker Int where

class BracedMarker a where {}

instance BracedMarker Int where {}

data LayoutEmpty where

data BracedEmpty where {}

requiresLayoutMarker :: LayoutMarker a => a -> Int
requiresLayoutMarker _ = 20

requiresBracedMarker :: BracedMarker a => a -> Int
requiresBracedMarker _ = 22

ok = requiresLayoutMarker (1 :: Int) + requiresBracedMarker (2 :: Int) == 42

main = print (if ok then (1 :: Int) else 0)
