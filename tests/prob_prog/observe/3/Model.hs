module Model where

import           Probability.Random
import           Probability.Distribution.Normal

model = do
  x <- sample $ normal 0 1
  y <- sample $ normal x 1
  return []

main = do
  return model
