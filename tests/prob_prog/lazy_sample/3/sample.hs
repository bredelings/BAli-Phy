module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

model x = do
  x <- sample $ normal 0.0 1.0
  ys <- lazy $ sample $ independent $ repeat $ normal 0.0 1.0
  let (mu,sigma) = (x, sum $ take 10 $ map (^2) ys)
  observe x $ normal mu sigma
  return [ "mu" %=% mu, "sigma" %=% sigma]

main = do
  options <- execParser $ modelRunParser "sample" 200000
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) $ model 1

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
