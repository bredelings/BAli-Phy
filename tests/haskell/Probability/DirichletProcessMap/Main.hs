{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq ((==))
import Data.Function (($))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Probability.Distribution.DirichletProcess (dirichletProcessMap)
import Probability.Distribution.Discrete (delta)
import Probability.Random (runRandomLazy)
import System.IO (IO)

-- Fail the test with a focused explanation when a map-indexed DP invariant is violated.
assert condition message = if condition then return () else error message

-- Check that every requested integer key is present and that an empty set stays empty.
main :: IO ()
main = do
  keyed <- runRandomLazy $ dirichletProcessMap (IS.fromList [7, 2, 9]) 1 (delta 4)
  empty <- runRandomLazy $ dirichletProcessMap IS.empty 1 (delta 4)
  assert (IM.size keyed == 3) "dirichletProcessMap returned the wrong number of keys"
  assert (IM.member 7 keyed && IM.member 2 keyed && IM.member 9 keyed)
         "dirichletProcessMap did not retain every requested key"
  assert (keyed IM.! 7 == 4 && keyed IM.! 2 == 4 && keyed IM.! 9 == 4)
         "dirichletProcessMap associated a value with the wrong key"
  assert (IM.size empty == 0) "dirichletProcessMap did not preserve an empty domain"
