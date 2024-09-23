module Test where

import           Probability

model = do
    xs <- prior $ iid 10 (categorical [0.1, 0.2, 0.3, 0.4])
    return ["xs" %=% xs]

main logDir = do
  return model
