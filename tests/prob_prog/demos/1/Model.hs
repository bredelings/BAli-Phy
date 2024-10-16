module Model where

import           Probability

model = do

    p <- prior $ beta 5.0 1.0

    n <- prior $ geometric p

    return ["p" %=% p, "n" %=% n]

main logDir = do
  return model
