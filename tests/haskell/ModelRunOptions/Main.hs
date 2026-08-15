{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BAliPhy.Run
import Compiler.Base
import Data.Function
import Options.Applicative
import System.IO
import Text.Show

-- Protect the common run-mode precedence independently of generated models and filesystem effects.
-- This becomes obsolete if standalone model programs stop using ModelRunOptions.
main = do
    options <- execParser $ modelRunParser "default-run" 10
    putStrLn $ show $ runMode options
