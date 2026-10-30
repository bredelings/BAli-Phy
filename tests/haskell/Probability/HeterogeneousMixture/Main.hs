{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Compiler.Fractional
import Compiler.Error (error)
import Probability.Dist (HasPdf(pdf))
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.Mixture
import Probability.Distribution.Normal (normal, Normal)
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
    boundaryValue <- runRandomStrict (sample boundary)
    precedenceValue <- runRandomStrict (sample precedence)
    print [firstValue, middleValue, finalValue, singleValue, boundaryValue, precedenceValue]
    -- Density traversal must normalize globally and ignore inactive components, even an undefined tail.
    -- Selection tests alone do not catch suffix normalization or eager zero-weight evaluation.
    print [ pdf (3 .*. delta 40) 40
          , pdf (2 .*. delta 10 |+| 6 .*. delta 20) 10
          , pdf (20 .*. delta 10 |+| 60 .*. delta 20) 10
          , pdf (1 .*. delta 10 |+| 0 .*. (error "inactive" :: Normal)) 10
          , pdf (0 .*. (error "inactive" :: Normal) |+| 1 .*. delta 20) 20
          ]
