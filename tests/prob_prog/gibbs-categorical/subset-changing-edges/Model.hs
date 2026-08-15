module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

-- Make every categorical alternative change the number of downstream random variables.
model = do
  i <- prior $ categorical (replicate 50 0.02)
  xs <- prior $ iid i (uniform 0 1)
  return ["i" %=% i, "total" %=% sum xs]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
