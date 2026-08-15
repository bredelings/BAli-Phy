module LinearRegression where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Data.Frame

model xs ys = do

    b     <- prior $ normal 0 1

    a     <- prior $ normal 0 1

    sigma <- prior $ exponential 1

    let f x = b * x + a

    observe ys $ independent [ normal (f x) sigma | x <- xs ]

    return ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

main = do
  options <- execParser $ modelRunParser "LinearRegression" 200000

  runInfo <- initializeModelRun (runMode options)

  xy_data <- readTable "xy.csv"

  let xs = xy_data $$ "x" :: [Double]
      ys = xy_data $$ "y" :: [Double]

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model xs ys

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
