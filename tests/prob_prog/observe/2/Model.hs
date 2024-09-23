module Model where

import           Probability.Random
import           Probability.Distribution.Normal

observe_data z' = do
    x <- prior $ normal 0 1
    y <- prior $ normal x 1
    observe z'$ normal y 1
    return ["x" %=% x, "y" %=% y]

main logDir = do
  let model = observe_data 1

  return model
