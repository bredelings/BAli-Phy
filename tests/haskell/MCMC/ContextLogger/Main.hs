{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (return)
import Data.Function (($))
import Data.JSON (Key)
import MCMC (logPosterior, runMCMC)
import Probability.Logger (makeJSONLogger)
import Probability.Random
  (LoggerValues(..), (%=%), (%=!), (%>!), addLogger, contextFields, makeMCMCModel,
   parameterLogValues)
import System.IO (IO, stdout)

-- Register a scalar logger with both ordinary and context-dependent fields.
model = do
  addLogger $ makeJSONLogger stdout loggerValues
  return (parameterLogValues loggerValues)
  where
    loggerValues =
      LoggerValues
        [("answer" :: Key) %=% (7 :: Int)]
        (contextFields ["nested" %>! ("posterior" %=! logPosterior)])

main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 1 context
