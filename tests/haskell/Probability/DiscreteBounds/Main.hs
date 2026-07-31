{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Probability.Dist (Dist1D(lower_bound, upper_bound))
import Probability.Distribution.Discrete (Discrete(..), delta)
import System.IO (print)

-- Discrete support bounds include every listed value, even one whose
-- probability is zero, while an empty distribution has no reported bounds.
main = print
    ( ( (lower_bound singleton, upper_bound singleton)
      , (lower_bound multiple, upper_bound multiple)
      )
    , ( (lower_bound zeroProbability, upper_bound zeroProbability)
      , (lower_bound empty, upper_bound empty)
      , (lower_bound integers, upper_bound integers)
      )
    )
  where
    singleton = delta 4
    multiple = Discrete [(3, 0.25), (-2, 0.5), (8, 0.25)] :: Discrete Double
    zeroProbability = Discrete [(-10, 0), (3, 1)] :: Discrete Double
    empty = Discrete [] :: Discrete Double
    integers = Discrete [(4, 0.5), (-3, 0.5)] :: Discrete Int
