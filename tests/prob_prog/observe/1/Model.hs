module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability.Random
import           Probability.Distribution.Normal

observe_data x = do
    observe x $ normal 0 1
    return []

main = do
  options <- execParser $
    info
      (modelRunOptions "Model" 200000 (pure ()) <**> helper)
      (fullDesc <> progDesc "Run the observation example")

  runInfo <- initializeModelRun (testMode options) (outputName options)

  let model = observe_data 1

  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
