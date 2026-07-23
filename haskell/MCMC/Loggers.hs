module MCMC.Loggers where

import           Numeric.LogDouble
import           Numeric.Prob (fromLogOdds, toFloating)
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

foreign import trcall "MCMC:condLogOddsRaw" condLogOddsValuesRaw
  :: Modifiable Int -> Int -> ContextIndex -> IO (U.Vector Double)

condLogOddsValues :: Modifiable Int -> Int -> ContextAction (U.Vector Double)
condLogOddsValues selector count = ContextAction (condLogOddsValuesRaw selector count)

-- Select one state's log odds from the complete conditional distribution.
condLogOdds :: Modifiable Int -> Int -> Int -> ContextAction Double
condLogOdds selector target count = do
  values <- condLogOddsValues selector count
  return (values U.! target)

-- Convert the selected state's log odds to an ordinary probability.
condPr :: Modifiable Int -> Int -> Int -> ContextAction Double
condPr selector target count = do
  value <- condLogOdds selector target count
  return (toFloating (fromLogOdds value) :: Double)
