module UnicodeOps ((⊕), Pair(..), pairSum) where

(⊕) :: Int -> Int -> Int
x ⊕ y = x + y

data Pair = Int :⊕ Int

pairSum :: Pair -> Int
pairSum (x :⊕ y) = x + y
