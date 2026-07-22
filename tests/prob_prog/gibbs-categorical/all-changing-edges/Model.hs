module Model where

import Probability

-- Adds one sampled variable at every edge of the categorical candidate chain.
model = do
  i <- prior $ categorical (replicate 10 0.1)
  xs <- prior $ iid i (uniform 0 1)
  return ["i" %=% i, "total" %=% sum xs]

main _ = return model
