module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model z' = do
    x  <- sample $ normal 0 1
    ys <- lazy $ sample $ independent $ repeat $ normal 0 1
    let zs = (x * x) : (take 10 ys)
    observe z' $ normal (zs !! 2) 1
    return ["zs" %=% zs]

main = do
  options <- execParser $ modelRunParser "sample" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model 10

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
