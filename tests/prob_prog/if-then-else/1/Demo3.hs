module Demo3 where

import           Probability

main = sample $ do
    i <- bernoulli 0.5
    y <- normal 0.0 1.0
    z <- exponential 0.1
    let x = if i == 1 then y else z
    return ["x" %=% x]

