module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
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
  (options, filename) <- execParser $
    modelRunParserWith "Model" 200000 $
      strArgument (metavar "TABLE" <> help "Table containing an x column")

  runInfo <- initializeModelRun (testMode options) (outputName options)

  xtable <- readTable filename

  let xs = xtable $$ "x" :: [Double]

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model xs

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
