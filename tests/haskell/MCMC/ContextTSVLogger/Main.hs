{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (return)
import Data.Function (($))
import Data.JSON (Key)
import qualified Data.Text.IO as T
import MCMC (logPosterior, runMCMC)
import Probability.Logger (tsvLogger)
import Probability.Random
  (LoggerValues(..), (%=%), (%=!), (%>!), addLogger, contextFields, makeMCMCState,
   parameterLogValues)
import System.IO (IO)

-- Register a TSV logger with both ordinary and context-dependent fields.
model logger = do
  addLogger $ logger loggerValues
  return (parameterLogValues loggerValues)
  where
    loggerValues =
      LoggerValues
        [("answer" :: Key) %=% (7 :: Int)]
        (contextFields ["nested" %>! ("posterior" %=! logPosterior)])

main :: IO ()
main = do
  logger <- tsvLogger "obtained-samples.tsv" ["iter"]
  mcmcState <- makeMCMCState (model logger)
  runMCMC 1 mcmcState
  samples <- T.readFile "obtained-samples.tsv"
  columnNames <- T.readFile "obtained-samples.tsv.column-map.json"
  T.putStr samples
  T.putStrLn columnNames
