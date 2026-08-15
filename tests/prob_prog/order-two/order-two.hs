module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

random_walk x1 x2 = do
    dx <- sample $ normal 0 1
    let x3 = dx - x1 + (2 * x2)
    xs <- random_walk x2 x3
    return (x1 : xs)

model = do
    x1   <- sample $ normal 0 1
    x2   <- sample $ normal x1 (sqrt $ 1 / 3)
    walk <- lazy $ random_walk x1 x2
    let xs = take 100 walk
    return ["x" %=% xs]

main = do
  options <- execParser $ modelRunParser "order-two" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
