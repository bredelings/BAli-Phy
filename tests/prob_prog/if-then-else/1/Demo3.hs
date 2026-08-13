module Demo3 where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

model = do
    i <- prior $ bernoulli 0.5
    y <- prior $ normal 0 1
    z <- prior $ exponential 0.1
    let x = if i == 1 then y else z
    return ["x" %=% x]

main = do
  options <- execParser $ modelRunParser "Demo3" 200000
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
