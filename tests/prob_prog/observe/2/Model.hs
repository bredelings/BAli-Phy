module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability.Random
import           Probability.Distribution.Normal

observe_data z' = do
    x <- prior $ normal 0 1
    y <- prior $ normal x 1
    observe z'$ normal y 1
    return ["x" %=% x, "y" %=% y]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)
  let model = observe_data 1

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
