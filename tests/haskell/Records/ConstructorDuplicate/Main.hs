module Main where

data Point = Point { x :: Int, y :: Int }

p = Point { x = 1, x = 2, y = 3 }

main = print (x p)
