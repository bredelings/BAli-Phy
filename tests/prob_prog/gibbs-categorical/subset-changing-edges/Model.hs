module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

-- Make every categorical alternative change the number of downstream random variables.
model = do
  i <- prior $ categorical (replicate 50 0.02)
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

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
