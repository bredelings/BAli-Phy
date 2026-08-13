module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

-- Adds one sampled variable at every edge of the categorical candidate chain.
model = do
  i <- prior $ categorical (replicate 10 0.1)
  xs <- prior $ iid i (uniform 0 1)
  return ["i" %=% i, "total" %=% sum xs]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context
