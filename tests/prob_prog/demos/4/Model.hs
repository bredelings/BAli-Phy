module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model = do

    xs         <- prior $ iid 10 (normal 0.0 1.0)

    categories <- prior $ iid 10 (categorical (replicate 10 0.1))

    let ys = [ xs !! (categories !! i) | i <- [0 .. 9] ]
    return ["ys" %=% ys]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
