{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.Mixture
import Probability.Distribution.Normal (normal)
import Probability.Random (runRandomStrict, sample)
import System.IO (IO, print)

-- Exercise every recursive selection path with heterogeneous component types
-- and deterministic weights, including the exact-zero boundary.
main :: IO ()
main = do
    let first = 2 .*. delta 10 |+| 0 .*. normal 0 1 |+| 0 .*. delta 30
        middle = 0 .*. normal 0 1 |+| 7 .*. delta 20 |+| 0 .*. delta 30
        final = 0 .*. delta 10 |+| 0 .*. normal 0 1 |+| 5 .*. delta 30
        single = 3 .*. delta 40
        boundary = 0 .*. normal 0 1 |+| 1 .*. delta 50
        precedence = 0 .*. normal 0 1 |+| 0 .*. delta 0 |+| 1 - 0 - 0 .*. delta 60
    firstValue <- runRandomStrict (sample first)
    middleValue <- runRandomStrict (sample middle)
    finalValue <- runRandomStrict (sample final)
    singleValue <- runRandomStrict (sample single)
    boundaryValue <- runRandomStrict (sampleAt 0 boundary)
    precedenceValue <- runRandomStrict (sample precedence)
    print [firstValue, middleValue, finalValue, singleValue, boundaryValue, precedenceValue]
