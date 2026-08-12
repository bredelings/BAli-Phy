module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

-- Exercise changing n, changing alpha without changing n, and forcing every resolved output.
model = do
  dimensionChoice <- prior $ categorical (replicate 3 (1 / 3))
  alphaChoice <- prior $ categorical [0.5, 0.5]
  let n = 20 + dimensionChoice
      alpha = if alphaChoice == 0 then 0.5 else 2
  values <- dirichletProcess n alpha (normal 0 1)
  observe 0 $ normal (sum values / fromIntegral n) 1
  return ["n" %=% n, "alpha" %=% alpha, "values" %=% values]

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
