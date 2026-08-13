module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

-- Exercises categorical Gibbs sampling when every candidate has the same variables.
model = do
  i <- prior $ categorical (replicate 4 0.25)
  return ["i" %=% i]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
