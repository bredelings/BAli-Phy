module Model where

import           Probability

random_walk x1 x2 = do
    dx <- sample $ normal 0 1
    let x3 = dx - x1 + (2 * x2)
    xs <- random_walk x2 x3
    return (x1 : xs)

model = do
    x1   <- sample $ normal 0 1
    x2   <- sample $ normal x1 (sqrt $ 1 / 3)
    walk <- lazy $ random_walk x1 x2
    let xs = take 100 walk
    return ["x" %=% xs]

main logDir = do
  return model
