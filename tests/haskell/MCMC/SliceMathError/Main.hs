{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Eq
import Data.OldList ((!!))
import Data.Ord
import Data.Tuple (fst)
import MCMC (runMCMC)
import MCMC.Moves.Real (sliceSample)
import Probability.Distribution.Discrete (unpackDiscrete)
import Probability.Random (addMove, condition, makeMCMCModel, modifiable)
import Range (between)
import SModel.ASRV (gammaRatesQuadrature)
import System.IO (IO, putStrLn)

-- Keep the initial point valid while making every distinct slice candidate invalid.
model = do
  let x = modifiable 0.1
      alpha = if x == 0.1 then 1.0 else -1.0
      firstRate = fst (unpackDiscrete (gammaRatesQuadrature alpha 2) !! 0)
  addMove 1 (sliceSample x (between (-1.0) 1.0))
  condition (firstRate > 0)
  return []

-- Invalid numerical candidates should have zero density rather than terminate the run.
main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 5 context
  putStrLn "completed"
