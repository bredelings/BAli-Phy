module Test where

import           Probability

main = random $ do
    xs <- iid 10 (categorical [0.1, 0.2, 0.3, 0.4])
    return ["xs" %=% xs]
