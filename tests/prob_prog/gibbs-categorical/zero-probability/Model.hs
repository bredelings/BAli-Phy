module Model where

import BAliPhy.Run
import Probability
import MCMC (gibbsSampleCategorical, runMCMC)
import Options.Applicative
import Probability.Random (writeTraceGraph)

-- Requires the selected candidate to retain the sampled value that made it possible.
model = do
  let i = modifiable 0
  addMove 1 $ gibbsSampleCategorical i 3
  x <- if i == 0 then return 0 else prior $ uniform 0 1
  condition (i /= 1 || x > 0.5)
  return ["i" %=% i, "x" %=% x]

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
