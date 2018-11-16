module Demo1 where

import Distributions

main = do

  p <- sample $ beta 5.0 1.0

  n <- sample $ geometric p

  return $ log_all [ p %% "p", n %% "n"]
