module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Data.Frame

generate size = do
  let w     = [0.35, 0.4, 0.25]
      mu    = [0.0, 2.0, 5.0]
      sigma = [0.5, 0.5, 1.0]
  xs <- sample $ iid size $ mixture w [ normal m s | (m,s) <- zip mu sigma ]
  return ["xs" %=% xs]


main_generate = generate 1000

model xs = do

  let n_components = 3

  w <- sample $ symmetricDirichlet n_components 1
  mu <- sort <$> sample (iid n_components (cauchy 0 1))
  tau <- sample $ iid n_components (gamma 1 1)

  let loggers = [ "dists" %=% zip w (zip mu tau) ]

  let n_points = length xs

  observe xs $ iid n_points (mixture w [ normal m s | (m, s) <- zip mu tau])

  return loggers

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  frame <- readTable "x.csv"

  let xs = frame $$ "x" :: [Double]

  context <- makeModelContext runInfo (logFormats options) $ model xs

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context
