{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq ((==))
import Data.Function (($))
import Probability.Distribution.DirichletProcess (dirichletProcessOn)
import Probability.Distribution.Discrete (delta)
import Probability.Random (runRandomLazy)
import System.IO (IO)

-- Fail the test with a focused explanation when an indexed DP invariant is violated.
assert condition message = if condition then return () else error message

-- Check that list keys are attached in caller order and that an empty domain stays empty.
main :: IO ()
main = do
  keyed <- runRandomLazy $ dirichletProcessOn [7, 2, 9] 1 (delta (4 :: Double))
  empty <- runRandomLazy $ dirichletProcessOn ([] :: [Int]) 1 (delta (4 :: Double))
  assert (keyed == [(7, 4), (2, 4), (9, 4)])
         "dirichletProcessOn did not preserve the supplied key order"
  assert (empty == ([] :: [(Int, Double)]))
         "dirichletProcessOn did not preserve an empty domain"
