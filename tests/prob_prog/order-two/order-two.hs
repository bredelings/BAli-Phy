module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

random_walk x1 x2 = do
    dx <- sample $ normal 0 1
    let x3 = dx - x1 + (2 * x2)
    xs <- random_walk x2 x3
    return (x1 : xs)

model = do
    x1   <- sample $ normal 0 1
    x2   <- sample $ normal x1 (sqrt $ 1 / 3)
    walk <- lazy $ random_walk x1 x2
    let xs = take 100 walk
    return ["x" %=% xs]

main = do
  options <- execParser $
    info (modelRunOptions "order-two" 200000 (pure ()) <**> helper) fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
