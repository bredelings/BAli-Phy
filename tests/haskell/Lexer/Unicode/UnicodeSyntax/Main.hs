{-# LANGUAGE ExplicitForall, UnicodeSyntax #-}
module Main where

import System.IO (print)

identity ∷ ∀ a . a → a
identity x = x

same ∷ Eq a ⇒ a → a → Bool
same x y = x == y

type F ∷ (★ → ★) → ★
data family F c

data instance F [] = FList

main = do
  x ← return (identity (5 ∷ Int))
  print (same x 5)
  print (case FList of FList -> True)
