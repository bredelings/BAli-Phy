module Model where

import Probability

-- Make every categorical alternative change the number of downstream random variables.
model = do
  i <- prior $ categorical (replicate 50 0.02)
  xs <- prior $ iid i (uniform 0 1)
  return ["i" %=% i, "total" %=% sum xs]

main _ = return model
