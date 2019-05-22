module Demo3 where

import Probability

main = random $ do
  i <- sample $ bernoulli 0.5
  y <- sample $ normal 0.0 1.0
  z <- sample $ exponential 0.1
  let x = if i==1 then y else z
  return $ log_all [x %% "x"]

