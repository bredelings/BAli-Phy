{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.Exit
import System.IO

-- Ensure an explicit exit unwinds through evaluation, suppresses later IO, and reaches the runner.
-- This can be removed if a future runtime supplies equivalent top-level ExitCode handling.
main = do
    putStrLn "before exit"
    exitWith (ExitFailure 7#)
    putStrLn "unreachable"

success :: IO a
success = exitSuccess

failure :: IO a
failure = exitFailure
