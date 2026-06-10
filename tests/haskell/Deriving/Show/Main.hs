{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Text.Show
import System.IO

data Color = Red | Blue | Green deriving Show
data Box a = Box a deriving Show
data Pair a b = Pair a b deriving Show
data Fancy = Fancy (Int, Color) [Int] [Char] Char deriving Show
data Point = Point { pointX :: Int, pointColor :: Color } deriving Show
infixr 5 :+:
data Expr = Lit Int | Expr :+: Expr deriving Show

main = do
  putStrLn (show Red)
  putStrLn (show (Box Blue))
  putStrLn (show (Pair Red (Box Blue)))
  putStrLn (showsPrec 11 (Box Blue) "")
  putStrLn (show (Fancy (1, Blue) [2,3] "hi" 'x'))
  putStrLn (show (Point 7 Blue))
  putStrLn (show (Lit 1 :+: (Lit 2 :+: Lit 3)))
