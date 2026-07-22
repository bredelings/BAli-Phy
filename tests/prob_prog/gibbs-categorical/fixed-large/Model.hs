module Model where

import Probability

-- Exercises retaining a larger collection of fixed-dimension candidates.
model = do
  i <- prior $ categorical (replicate 50 0.02)
  return ["i" %=% i]

main _ = return model
