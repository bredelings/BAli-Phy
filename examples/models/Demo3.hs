module Demo3 where

import Probability

main = do
  i <- sample $ bernoulli 0.5
  y <- sample $ normal 0.0 1.0
  let x = if (i==1) then y else 0.0
  return $ log_all [i %% "i", x %% "x"]

