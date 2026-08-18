{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq ((==))
import Data.Function (($))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Probability.Distribution.DirichletProcess (dirichletProcessOn)
import Probability.Distribution.Discrete (delta)
import Probability.Random (runRandomLazy)
import System.IO (IO)

-- Fail the test with a focused explanation when an indexed DP invariant is violated.
assert condition message = if condition then return () else error message

-- Check the Set-to-Map domain contract directly; full-model tests do not isolate
-- deterministic key ranking or the empty-domain case.
main :: IO ()
main = do
  keyed <- runRandomLazy $ dirichletProcessOn (Set.fromList [7, 2, 9]) 1 (delta (4 :: Double))
  empty <- runRandomLazy $ dirichletProcessOn (Set.empty :: Set.Set Int) 1 (delta (4 :: Double))
  assert (Map.toAscList keyed == [(2, 4), (7, 4), (9, 4)])
         "dirichletProcessOn did not return the supplied key set"
  assert (Map.null empty)
         "dirichletProcessOn did not preserve an empty domain"
