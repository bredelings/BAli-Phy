module Main where

import qualified UnicodeSupport as U
import System.IO (print)

λ :: Int
λ = 1

東京 :: Int
東京 = 2

aλ :: Int
aλ = 3

λ² :: Int
λ² = 4

-- The name is lambda followed by U+0301 COMBINING ACUTE ACCENT.
λ́ :: Int
λ́ = 5

data Δ = Δ

fromΔ :: Δ -> Int
fromΔ Δ = 6

main = do
  print (λ + 東京 + aλ + λ² + λ́ + fromΔ Δ + U.λ + U.東京)
  print (case U.Δ of U.Δ -> True)
