module LiStephens2003 where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Probability         -- for the model framework

import           Options.Applicative
import           Data.Frame          -- for readTable & friends
import           Bio.Alignment       -- for load_alignment
import           Bio.Alphabet        -- for dna
import           PopGen              -- for li_stephens_2003

model locs sequence_data = do

  rho <- sample $ logLaplace 0.01 2

  let len = 1 + fromIntegral (last locs)

  observe sequence_data $ li_stephens_2003 locs [(rho,0,len)]

  return ["rho" %=% rho ]

data ModelInputs = ModelInputs
  { sequenceFile :: FilePath
  , locationsFile :: FilePath
  }

modelInputOptions =
  ModelInputs
    <$> strArgument (metavar "SEQUENCES" <> help "Aligned DNA sequences")
    <*> strArgument (metavar "LOCATIONS" <> help "Table of sequence locations")

main = do
  (options, inputs) <- execParser $
    withModelDescription "Run the Li-Stephens recombination model" $
      modelRunParserWith "LiStephens2003_1" 200000 modelInputOptions

  runInfo <- initializeModelRun (runMode options)

  sequence_data <- load_alignment dna (sequenceFile inputs)

  locs_table <- readTable (locationsFile inputs)
  let locs = locs_table $$ "locs" :: [Int]

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model locs sequence_data

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
