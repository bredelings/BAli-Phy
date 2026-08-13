{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base
import Compiler.Num
import Control.Applicative
import Data.Function
import Data.Functor
import Data.Semigroup
import Data.Tuple
import Options.Applicative
import System.IO

parser :: Parser (Int, String, String)
parser = (,,)
    <$> option auto (long "count" <> short 'c' <> metavar "N" <> value 1 <> showDefault <>
                     help "Number of passes")
    <*> strArgument (metavar "FILE" <> help "Input file")
    <*> strOption (long "mode" <> metavar "MODE" <> help "Processing mode")

parserInfo :: ParserInfo (Int, String, String)
parserInfo = info (parser <**> helper)
    (fullDesc <> header "Example tool" <> progDesc "Process one file")

nestedParserInfo :: ParserInfo String
nestedParserInfo = info (subparser (command "outer" (info inner idm))) idm where
    inner = subparser (command "inner" (info
        (strOption (long "name" <> metavar "NAME")) idm))

renderResult :: ParserResult a -> String
renderResult (Failure failure) = fst (renderFailure failure "bali-phy")
renderResult (Success _) = "unexpected success"

-- Protect structural requiredness and canonical option-name ordering, which pure result tests
-- cannot observe.
-- This becomes obsolete when the upstream help integration tests can run unchanged.
main = do
    putStrLn (renderResult (execParserPure defaultPrefs parserInfo ["--help"]))
    putStrLn "\n---"
    putStrLn (renderResult (execParserPure defaultPrefs nestedParserInfo ["outer", "inner"]))
