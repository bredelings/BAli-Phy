module Main where

import System.IO (print)

(→) :: Int -> Int -> Int
x → y = x + y

(∷) :: Int -> Int -> Int
x ∷ y = x * y

main = do
  print ((1 → 2) == 3)
  print ((2 ∷ 3) == 6)
