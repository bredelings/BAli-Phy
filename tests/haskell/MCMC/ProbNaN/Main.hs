{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.Function (($))
import MCMC (runMCMC)
import MCMC.Moves.Integer (discreteUniformAvoidMH)
import Probability.Distribution.Bernoulli (bernoulli)
import Probability.Random (addMove, makeMCMCModel, modifiable, prior, toProb)
import System.IO (IO, putStrLn)

-- Exercise NaN as both an existing variable's density and a newly sampled
-- variable's parameter; both proposed contexts should be rejected.
model = do
  let densitySelector = modifiable (0 :: Int)
      samplingSelector = modifiable (0 :: Int)
  let probability = if densitySelector == 0 then 0.5 else 0/0
  _ <- prior $ bernoulli $ toProb probability
  addMove 1 $ discreteUniformAvoidMH densitySelector 0 1
  addMove 1 $ discreteUniformAvoidMH samplingSelector 0 1
  if samplingSelector == 0
    then return []
    else do
      _ <- prior $ bernoulli (0/0)
      return []

-- Completion proves that both invalid-proposal paths are recoverable.
main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 20 context
  putStrLn "completed"
