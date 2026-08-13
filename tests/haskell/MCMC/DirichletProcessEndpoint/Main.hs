{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.Function (($))
import Data.OldList (sum)
import Data.Ord
import MCMC (runMCMC)
import Probability.Distribution.Categorical (categorical)
import Probability.Distribution.DirichletProcess (dirichletProcess)
import Probability.Distribution.Normal (normal)
import Probability.Random (condition, makeMCMCState, prior)
import System.IO (IO, putStrLn)

-- Force every resolved atom while allowing the selector to propose transitions
-- between an ordinary concentration and the infinite-concentration endpoint.
model = do
  endpoint <- prior $ categorical [0.5, 0.5]
  let alpha = if endpoint == 0 then 1 else 1 / 0
  values <- dirichletProcess 4 alpha (normal 0 1)
  condition (sum values < 1 / 0)
  return []

-- Completion exercises initialization and repeated density evaluation at both concentrations.
main :: IO ()
main = do
  mcmcState <- makeMCMCState model
  runMCMC 100 mcmcState
  putStrLn "completed"
