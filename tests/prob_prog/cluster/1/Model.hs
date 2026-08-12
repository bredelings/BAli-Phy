module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)
import Data.Frame

-- Ideally, the categories and their weights would be exchangeable!
-- Currently if the first category is bad, there is not a good way
-- to eliminate it.

cluster_dist = do
  mean <- prior $ normal 0 10
  prec <- prior $ gamma 2 1
  let sigma = 1/prec
  return (mean,sigma)

model xs = do

  let n_points = length xs

  let alpha = 0.5

  params <- prior $ dp n_points alpha cluster_dist

  observe xs $ independent [normal mean sigma | (mean,sigma) <- params]

  let loggers = ["alpha" %=% alpha, "params" %=% params]

  return loggers


main = do
  options <- execParser $
    info
      (modelRunOptions "Model" 200000
        (strArgument (metavar "TABLE" <> help "Table containing an x column")) <**> helper)
      fullDesc
  run <- prepareModelRun (testMode options) (outputName options)

  xtable <- readTable (modelInputs options)

  let xs = xtable $$ "x" :: [Double]

  context <- makeModelContext run (logFormats options) $ model xs

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
