module Model where

import BAliPhy.Run
import Probability
import MCMC
import Options.Applicative
import Probability.Random (writeTraceGraph)

model = do

  -- Default moves have a rate of 0.
  x <- RanSamplingRate 0 $ sample $ normal 0 1

  -- Force x ... so that the transition kernels happen?
  condition (x > 0 || x <= 0)

  -- Try out the generic proposal for atomic objects.
  addMove 10 $ metropolisHastings $ propose x (\x -> normal (x+1) 1)

  -- Log x
  return ["x" %=% x]

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
