{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.OldList ((!!))
import Data.Ord
import Data.Tuple (fst)
import MCMC (runMCMC)
import MCMC.Moves.Integer (discreteUniformAvoidMH)
import Probability.Distribution.Discrete (unpackDiscrete)
import Probability.Random (addMove, condition, makeMCMCModel, modifiable)
import SModel.ASRV (gammaRatesQuadrature, logNormalRatesQuadrature)
import System.IO (IO, putStrLn)

-- Make both numerical proposals invalid while keeping their initial values valid.
model = do
  let gammaChoice = modifiable (0 :: Int)
      logNormalChoice = modifiable (0 :: Int)
      alpha = if gammaChoice == 0 then 1.0 else -1.0
      logMean = if logNormalChoice == 0 then 0.0 else -1000.0
      firstRate = fst (unpackDiscrete (gammaRatesQuadrature alpha 2) !! 0)
      firstLogNormalRate = fst (unpackDiscrete (logNormalRatesQuadrature logMean 0.0 2) !! 0)
  addMove 1 (discreteUniformAvoidMH gammaChoice 0 1)
  addMove 1 (discreteUniformAvoidMH logNormalChoice 0 1)
  condition (firstRate > 0)
  condition (firstLogNormalRate > 0)
  return []

-- Repeated numerical failures should reject proposals without terminating the run.
main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 20 context
  putStrLn "completed"
