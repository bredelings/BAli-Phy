module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)
import Data.Frame

cluster_dist = do
  mean <- sample $ cauchy 0.0 1.0
  sigma <- sample $ exponential 1.0
  return (mean, sigma)

model xs = do

  n <- (1+) <$> sample (geometric 0.33)

  clusters <- sample $ iid n cluster_dist

  ps <- sample $ symmetricDirichlet n 0.5

  let n_points = length xs
      dists = [normal mean sigma | (mean,sigma) <- clusters]

  observe xs $ iid n_points (mixture ps dists)

  return ["n_clusters" %=% n, "weights" %=% ps, "clusters" %=% clusters]

main = do
  options <- execParser $
    info
      (modelRunOptions "Model" 200000
        (strArgument (metavar "TABLE" <> help "Table containing an x column")) <**> helper)
      fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)

  xtable <- readTable (modelInputs options)

  let xs = xtable $$ "x" :: [Double]

  context <- makeModelContext runInfo (logFormats options) $ model xs

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
