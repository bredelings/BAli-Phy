module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

-- Exercises retaining a larger collection of fixed-dimension candidates.
model = do
  i <- prior $ categorical (replicate 50 0.02)
  return ["i" %=% i]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
