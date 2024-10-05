module MCMC.Loggers where

import           Numeric.LogDouble
import           MCMC.Types (ContextIndex)
import           Data.JSON
import           Effect (Effect)
import qualified Data.Text as T

--- The first four arguments allow giving the logger the generation number, prior, likelihood, and probability.
type LoggerAction = Int -> Double -> Double -> Double -> IO ()

foreign import bpcall "MCMC:" registerLogger :: LoggerAction -> IO Effect

foreign import bpcall "MCMC:" logJSONRaw :: Int -> Int -> IO CJSON
logJSONLine context iter = cjsonToText <$> logJSONRaw context iter
foreign import bpcall "MCMC:" jsonToTableLineRaw :: CJSON -> CPPString
logTableLine context iter = T.fromCppString . jsonToTableLineRaw <$> logJSONRaw context iter

foreign import bpcall "MCMC:" prior :: ContextIndex -> IO LogDouble
foreign import bpcall "MCMC:" likelihood :: ContextIndex -> IO LogDouble
foreign import bpcall "MCMC:" posterior :: ContextIndex -> IO LogDouble
