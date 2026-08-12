module GaussianMixture where

-- See https://github.com/probmods/webppl/blob/dev/examples/gaussianMixture.wppl
import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

makeGaussian dim = do
  means <- replicateM dim (sample $ uniform 20.0 300.0)
  stds <- replicateM dim (sample $ uniform 5.0 50.0)
  return [normal mean std | (mean,std) <- zip means stds]

model = do
  mixtureWeight <- sample $ uniform 0.0 1.0
  gaussian1 <- makeGaussian 2
  gaussian2 <- makeGaussian 2

  let gaussianMixture = do
          c <- sample $ bernoulli 0.5
          if c == 1 then
              sample $ independent gaussian1
          else
              sample $ independent gaussian2

  x <- replicateM 100 gaussianMixture

  return ["x" %=% x]

main = do
  options <- execParser $
    info (modelRunOptions "GaussianMixture" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) model

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
