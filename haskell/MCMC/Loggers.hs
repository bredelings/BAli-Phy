module MCMC.Loggers where

import           Numeric.LogDouble
import           Compiler.Floating (ln)
import           MCMC.Types (ContextAction(..), ContextIndex, Modifiable)
import           Data.JSON
import qualified Data.Vector.Unboxed as U
import           Effect (Effect)
import qualified Data.Text as T

type GetContextFields = Int -> ContextIndex -> IO CJSON
type LogSample = Int -> CJSON -> IO ()
type LoggerAction = (GetContextFields, LogSample)

foreign import bpcall "MCMC:" registerLogger :: LoggerAction -> IO Effect

foreign import bpcall "MCMC:" logJSONRaw :: Int -> Int -> IO CJSON
logJSONLine context iter = cjsonToText <$> logJSONRaw context iter
foreign import bpcall "MCMC:" jsonToTableLineRaw :: CJSON -> CPPString
logTableLine context iter = T.fromCppString . jsonToTableLineRaw <$> logJSONRaw context iter

foreign import bpcall "MCMC:prior" priorRaw :: ContextIndex -> IO LogDouble
foreign import bpcall "MCMC:likelihood" likelihoodRaw :: ContextIndex -> IO LogDouble
foreign import bpcall "MCMC:posterior" posteriorRaw :: ContextIndex -> IO LogDouble

prior :: ContextAction LogDouble
prior = ContextAction priorRaw

likelihood :: ContextAction LogDouble
likelihood = ContextAction likelihoodRaw

posterior :: ContextAction LogDouble
posterior = ContextAction posteriorRaw

logPrior :: ContextAction Double
logPrior = ln <$> prior

logLikelihood :: ContextAction Double
logLikelihood = ln <$> likelihood

logPosterior :: ContextAction Double
logPosterior = ln <$> posterior

foreign import trcall "MCMC:condPrRaw" condPrsRaw
  :: Modifiable Int -> Int -> ContextIndex -> IO (U.Vector Double)

condPrs :: Modifiable Int -> Int -> ContextAction (U.Vector Double)
condPrs selector count = ContextAction (condPrsRaw selector count)

-- Select one probability from the complete conditional distribution computed by the builtin.
condPr :: Modifiable Int -> Int -> Int -> ContextAction Double
condPr selector target count = do
  probabilities <- condPrs selector count
  return (probabilities U.! target)
