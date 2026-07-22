module Model where

import Probability

-- Exercises categorical Gibbs sampling when every candidate has the same variables.
model = do
  i <- prior $ categorical (replicate 4 0.25)
  return ["i" %=% i]

main _ = return model
