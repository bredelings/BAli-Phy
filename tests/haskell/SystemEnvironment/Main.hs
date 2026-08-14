{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Eq
import Data.Maybe
import System.Environment
import System.IO

-- Protect argument, program-name, and optional-environment access on every native runner.
-- This becomes obsolete only if System.Environment is supplied by an upstream base package.
main = do
    name <- getProgName
    arguments <- getArgs
    present <- lookupEnv "PATH"
    missing <- lookupEnv "BALIPHY_EXPECTED_MISSING_7DB6307F"
    case (name == "Main", arguments, present, missing) of
        (True, [], Just _, Nothing) -> putStrLn "environment ok"
        _ -> putStrLn "environment bad"
