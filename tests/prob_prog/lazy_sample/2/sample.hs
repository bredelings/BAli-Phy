module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

model z' = do
    x  <- sample $ normal 0 1
    ys <- lazy $ sample $ independent $ repeat $ normal 0 1
    let zs = (x * x) : (take 10 ys)
    observe z' $ normal (zs !! 2) 1
    return ["zs" %=% zs]

main = do
  options <- execParser $
    info (modelRunOptions "sample" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) $ model 10

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
