module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability.Random
import           Probability.Distribution.Normal

model = do
  x <- sample $ normal 0 1
  y <- sample $ normal x 1
  return []

main = do
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) model

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
