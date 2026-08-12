module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

model x = do

    n <- sample $ geometric 0.33

    y <- if n > 1 then sample $ normal 0 1 else sample $ exponential 1

    observe x $ normal y 1

    return ["n" %=% n, "y" %=% y]

main = do
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) $ model 3

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
