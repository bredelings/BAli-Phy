module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
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
  (options, filename) <- execParser $
    modelRunParserWith "Model" 200000 $
      strArgument (metavar "TABLE" <> help "Table containing an x column")

  runInfo <- initializeModelRun (runMode options)

  xtable <- readTable filename

  let xs = xtable $$ "x" :: [Double]

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model xs

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
