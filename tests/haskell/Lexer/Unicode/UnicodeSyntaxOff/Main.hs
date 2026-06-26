module Main where

import qualified UnicodeSyntaxOffSupport as U
import System.IO (print)

(→) :: Int -> Int -> Int
x → y = x + y

(∷) :: Int -> Int -> Int
x ∷ y = x * y

main = do
  print ((1 → 2) == 3)
  print ((2 ∷ 3) == 6)
  print ((1 U.→ 2) == 3)
