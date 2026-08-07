{-# LANGUAGE NoImplicitPrelude #-}
module System.Exit (ExitCode(..), exitWith, exitSuccess, exitFailure) where

import Compiler.IO
import Data.Eq
import Data.Ord
import Text.Read
import Text.Show

data ExitCode = ExitSuccess | ExitFailure Int
    deriving (Eq, Ord, Read, Show)

foreign import bpcall "Environment:" exitWithRaw :: ExitCode -> IO a

exitWith :: ExitCode -> IO a
exitWith = exitWithRaw

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1#)
