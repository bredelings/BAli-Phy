module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

model = do

    p <- prior $ beta 5.0 1.0

    n <- prior $ geometric $ toProb p

    return ["p" %=% p, "n" %=% n]

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
