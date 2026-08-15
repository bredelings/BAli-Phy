module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

-- Creates a variable across one edge of the categorical candidate chain.
model = do
  i <- prior $ categorical (replicate 4 0.25)
  x <- if i == 0 then return 0 else prior $ uniform 0 1
  return ["i" %=% i, "x" %=% x]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
