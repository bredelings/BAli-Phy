{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base
import Compiler.Num
import Control.Applicative
import Data.Function
import Data.Functor
import Data.Semigroup
import Options.Applicative

parser :: Parser (Int, String, String)
parser = (,,)
    <$> option auto (long "count" <> metavar "N" <> value 1 <> showDefault <> help "Number of passes")
    <*> strArgument (metavar "FILE" <> help "Input file")
    <*> strOption (long "mode" <> metavar "MODE" <> help "Processing mode")

parserInfo :: ParserInfo (Int, String, String)
parserInfo = info (parser <**> helper)
    (fullDesc <> header "Example tool" <> progDesc "Process one file")

-- Protect structural requiredness and helper rendering, which pure result tests cannot observe.
-- This becomes obsolete when the upstream help integration tests can run unchanged.
main = handleParseResult (execParserPure defaultPrefs parserInfo ["--help"])
