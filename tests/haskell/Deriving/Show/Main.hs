{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Text.Show
import System.IO

data Color = Red | Blue | Green deriving Show
data Box a = Box a deriving Show
data Pair a b = Pair a b deriving Show

main = do
  putStrLn (show Red)
  putStrLn (show (Box Blue))
  putStrLn (show (Pair Red (Box Blue)))
  putStrLn (showsPrec 11 (Box Blue) "")
