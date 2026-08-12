module LinearRegression where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)
import           Data.Frame

model xs ys = do

    b     <- prior $ normal 0 1

    a     <- prior $ normal 0 1

    sigma <- prior $ exponential 1

    let f x = b * x + a

    observe ys $ independent [ normal (f x) sigma | x <- xs ]

    return ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

main = do
  options <- execParser $
    info (modelRunOptions "LinearRegression" 200000 (pure ()) <**> helper) fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)

  xy_data <- readTable "xy.csv"

  let xs = xy_data $$ "x" :: [Double]
      ys = xy_data $$ "y" :: [Double]

  context <- makeModelContext runInfo (logFormats options) $ model xs ys

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
