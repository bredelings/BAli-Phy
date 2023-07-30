module Demo3 where

import           Probability

model = do
    i <- prior $ bernoulli 0.5
    y <- prior $ normal 0 1
    z <- prior $ exponential 0.1
    let x = if i == 1 then y else z
    return ["x" %=% x]

main = do
  return model
