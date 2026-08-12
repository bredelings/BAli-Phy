module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

-- Exercises retaining a larger collection of fixed-dimension candidates.
model = do
  i <- prior $ categorical (replicate 50 0.02)
  return ["i" %=% i]

main = do
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
