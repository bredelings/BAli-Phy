{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base
import Compiler.Num
import Control.Applicative
import Data.Function
import Data.Functor
import Data.Semigroup
import Options.Applicative

parser :: Parser (Int, String)
parser = (,)
    <$> option auto (long "count" <> metavar "N" <> value 1 <> showDefault <> help "Number of passes")
    <*> strArgument (metavar "FILE" <> help "Input file")

parserInfo :: ParserInfo (Int, String)
parserInfo = info (parser <**> helper)
    (fullDesc <> header "Example tool" <> progDesc "Process one file")

-- Protect helper rendering and successful controlled exit without coupling this test to the
-- outer bali-phy command-line parser. System.Environment separately covers process arguments.
-- This can be removed when the upstream optparse-applicative integration suite is usable.
main = handleParseResult (execParserPure defaultPrefs parserInfo ["--help"])
