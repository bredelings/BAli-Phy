module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model fatalities = do

    alpha <- prior $ cauchy 0 1
    beta  <- prior $ cauchy 0 1

    let loggers = ["alpha" %=% alpha, "beta" %=% beta]

    -- Poisson regression with mass = e^(a + b*i)
    let dist i = poisson $ safe_exp (alpha + beta * (fromIntegral i))

    observe fatalities $ independent [ dist i | i <- [0 .. length fatalities - 1] ]

    return loggers

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  let fatalities = [24, 25, 31, 31, 22, 21, 26, 20, 16, 22]
      model' = model fatalities

  context <- makeModelContext runInfo (logFormats options) model'

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context
