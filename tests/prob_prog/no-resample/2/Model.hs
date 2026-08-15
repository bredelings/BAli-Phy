module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model x = do

    n <- sample $ geometric 0.33

    y <- if n > 1 then sample $ normal 0 1 else sample $ exponential 1

    observe x $ normal y 1

    return ["n" %=% n, "y" %=% y]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model 3

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
