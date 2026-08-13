module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

random_walk next x0 = lazy $ do
    x1 <- sample $ next x0
    xs <- random_walk next x1
    return (x0 : xs)

random_walk_n n next x0 = do
    xs <- random_walk next x0
    return (take n xs)

model = do

    p <- prior $ beta 10 1

    n <- prior $ geometric $ toProb p

    q <- prior $ cauchy 0 1

    x <- prior $ iid 10 (normal 0 1)

    c <- prior $ iid 10 (categorical (replicate 10 0.1))

    -- Random array indices.
    -- You can't do this in BUGS: it makes a dynamic graph!
    let w = [ x !! (c !! i) | i <- [0 .. 9] ]

    -- y[i] depends on x[i]
    y <- prior $ independent [ normal (x !! i) 1.0 | i <- [0 .. 9] ]

    -- Brownian-bridge-like
    z <- random_walk_n 10 (\mu -> normal mu 1) 0

    observe 2 $ normal (last z) 1

    return ["p" %=% p, "n" %=% n, "q" %=% q, "x" %=% x, "w" %=% w, "y" %=% y, "z" %=% z]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
