{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base
import Compiler.Classes
import Compiler.Num
import Control.Applicative
import Data.Bool
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Semigroup
import Options.Applicative
import System.IO
import Text.Show

permutationParser :: Parser ((Bool, Bool, Int), [String])
permutationParser = (\a b count_value files -> ((a, b, count_value), files))
    <$> switch (short 'a')
    <*> switch (short 'b')
    <*> option auto (short 'c' <> long "count")
    <*> many (strArgument (metavar "FILE"))

repeatedParser :: Parser [Int]
repeatedParser = many (option auto (short 'n' <> long "number"))

attachedParser :: Parser (Bool, String)
attachedParser = (,) <$> switch (short 'a') <*> strOption (short 'o')

alternativeParser :: Parser String
alternativeParser = flag' "on" (long "on") <|> flag' "off" (long "off")

commandParser :: Parser String
commandParser = subparser (command "add" (info (strArgument (metavar "FILE")) idm))

missingParser :: Parser Int
missingParser = option auto (long "required")

run :: Parser a -> [String] -> Maybe a
run parser = getParseResult . execParserPure defaultPrefs (info parser idm)

-- Protect the common pure parsing semantics where the local engine deliberately replaces the
-- upstream engine. This becomes obsolete when the upstream package's own tests can run unchanged.
main = putStrLn $ show
    ( ( run permutationParser ["left", "-abc7", "right"]
      , run repeatedParser ["-n1", "--number=2", "-n", "3"]
      , run attachedParser ["-oa"]
      )
    , ( run alternativeParser ["--off"]
      , run missingParser []
      )
    , ( run (strArgument (metavar "ARG")) ["--", "--literal"]
      , run commandParser ["add", "file"]
      )
    )
