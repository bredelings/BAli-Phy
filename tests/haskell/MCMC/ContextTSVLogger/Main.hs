{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (return)
import Data.Function (($))
import Data.IORef (newIORef)
import Data.JSON (Key)
import Data.Maybe (Maybe(..))
import MCMC (logPosterior, runMCMC)
import Probability.Logger (makeTSVLogger)
import Probability.Random
  (LoggerValues(..), (%=%), (%=!), (%>!), addLogger, contextFields, makeMCMCModel,
   parameterLogValues)
import System.IO (IO, stdout)

-- Register a TSV logger with both ordinary and context-dependent fields.
model state = do
  addLogger $ makeTSVLogger stdout ["iter"] state loggerValues
  return (parameterLogValues loggerValues)
  where
    loggerValues =
      LoggerValues
        [("answer" :: Key) %=% (7 :: Int)]
        (contextFields ["nested" %>! ("posterior" %=! logPosterior)])

main :: IO ()
main = do
  state <- newIORef Nothing
  context <- makeMCMCModel (model state)
  runMCMC 1 context
