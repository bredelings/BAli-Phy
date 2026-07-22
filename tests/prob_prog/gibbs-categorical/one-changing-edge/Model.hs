module Model where

import Probability

-- Creates a variable across one edge of the categorical candidate chain.
model = do
  i <- prior $ categorical (replicate 4 0.25)
  x <- if i == 0 then return 0 else prior $ uniform 0 1
  return ["i" %=% i, "x" %=% x]

main _ = return model
