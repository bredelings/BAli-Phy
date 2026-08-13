module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model = do

    xs <- prior $ iid 10 (normal 0 1)

    let ys = map (\x -> x * x) xs

    return ["xs" %=% xs, "squares" %=% ys, "sum" %=% sum ys]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
