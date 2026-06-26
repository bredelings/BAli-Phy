module Main where

import qualified UnicodeOps as U
import System.IO (print)

(⊗) :: Int -> Int -> Int
x ⊗ y = x * y

commented :: Int
commented = 4 -- this text must stay a comment

(--⊕) :: Int -> Int -> Int
x --⊕ y = x * 10 + y

data Pair = Int :⊗ Int

pairSum :: Pair -> Int
pairSum (x :⊗ y) = x + y

main = do
  print ((2 ⊗ 3) == 6)
  print ((U.⊕) 10 5 == 15)
  print (U.pairSum (1 U.:⊕ 2) == 3)
  print (pairSum (3 :⊗ 4) == 7)
  print (commented == 4)
  print ((1 --⊕ 2) == 12)
