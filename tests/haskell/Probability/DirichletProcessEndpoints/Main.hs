{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Fractional ((/))
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Function (($))
import Data.OldList (all, head, length, nub, null)
import Probability.Distribution.DirichletProcess (dirichletProcess)
import Probability.Distribution.Normal (normal)
import Probability.Random (runRandomLazy)
import System.IO (print)

allEqual values = all (\value -> value == head values) values

-- Verify that zero concentration reuses the first atom, while infinite
-- concentration creates every atom and both endpoints accept an empty sample.
main = do
    let sampleCount = 8
    allReused <- runRandomLazy $ dirichletProcess sampleCount 0 (normal 0 1)
    allFresh <- runRandomLazy $ dirichletProcess sampleCount (1 / 0) (normal 0 1)
    emptyZero <- runRandomLazy $ dirichletProcess 0 0 (normal 0 1)
    emptyInfinite <- runRandomLazy $ dirichletProcess 0 (1 / 0) (normal 0 1)
    print (allEqual allReused
        && length (nub allFresh) == sampleCount
        && null emptyZero
        && null emptyInfinite)
