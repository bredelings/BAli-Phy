module Main where

data Point = Point { x :: Int, y :: Int }

p = Point { x = 1, z = 2 }

main = print (x p)
