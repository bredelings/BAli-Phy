module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model = do
    i <- prior $ bernoulli 0.5
    y <- prior $ normal 0.0 1.0
    let x = if (i == 1) then y else 0.0
    return ["i" %=% i, "x" %=% x]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
