{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BAliPhy.Run
import Compiler.Base
import Control.Monad (return)
import Data.Function
import Options.Applicative

-- Verify exact-directory validation through the public initialization API.
-- This becomes obsolete if standalone model programs stop using ModelRunMode.
main = do
    options <- execParser $ modelRunParser "default-run" 10
    initializeModelRun (runMode options)
    return ()
