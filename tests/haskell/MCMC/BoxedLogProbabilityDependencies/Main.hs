{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Floating (Pow(ln), log)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Function (($))
import Data.Ord
import MCMC (runMCMC)
import MCMC.Loggers (likelihoodRaw)
import MCMC.Moves.Context (setAtomicModifiableValueInContext)
import MCMC.Types (TransitionKernel(..))
import Numeric.Prob (toProb)
import Probability.Distribution.Multinomial (multinomial)
import Probability.Random (addMove, makeMCMCState, modifiable, observe)
import System.IO (IO, putStrLn)

-- Check that changing a boxed vector replaces its dependent element USEs, so a
-- subsequent change to a newly selected element invalidates the native density.
-- This is obsolete if native distributions stop reading lazy boxed probability vectors.
checkDependencies selector probability = TransitionKernel (\context -> do
  setAtomicModifiableValueInContext selector 1 context
  firstDensity <- likelihoodRaw context
  setAtomicModifiableValueInContext probability 0.25 context
  secondDensity <- likelihoodRaw context
  let first = ln firstDensity
      second = ln secondDensity
  if abs (first - log 0.6) < 1.0e-10 && abs (second - log 0.25) < 1.0e-10
    then return ()
    else error "boxed log-probability dependencies were not updated")

model = do
  let selector = modifiable (0 :: Int)
      probability = modifiable (0.6 :: Double)
      probabilities = if selector == 0
                      then [0.8, 0.2]
                      else [toProb probability, 1 - toProb probability]
  observe [1, 0] $ multinomial 1 probabilities
  addMove 1 $ checkDependencies selector probability
  return []

main :: IO ()
main = do
  mcmcState <- makeMCMCState model
  runMCMC 1 mcmcState
  putStrLn "completed"
