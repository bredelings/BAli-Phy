{-# LANGUAGE NoImplicitPrelude #-}
module Options.Applicative.Extra
    ( execParserPure
    , execParser
    , customExecParser
    , handleParseResult
    , getParseResult
    , parserFailure
    , renderFailure
    , overFailure
    ) where

import Compiler.Base
import Compiler.Classes
import Control.Monad
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Help
import Options.Applicative.Internal
import Options.Applicative.Types
import System.Environment
import System.Exit
import System.IO

-- Compatibility note: upstream Extra also handles shell completion, terminal
-- width detection, parser suggestions, and the richer upstream help document.
getParseResult :: ParserResult a -> Maybe a
getParseResult (Success x) = Just x
getParseResult _ = Nothing

-- Run a parser without terminal IO so callers can inspect success and failure directly.
execParserPure :: ParserPrefs -> ParserInfo a -> [String] -> ParserResult a
execParserPure parser_prefs parser_info arguments =
    case runP (runParserInfo parser_info arguments) parser_prefs of
        (Right x, _) -> Success x
        (Left err, contexts) -> makeFailure parser_prefs parser_info contexts
            (if null arguments && prefShowHelpOnEmpty parser_prefs then ShowHelpText Nothing else err)

makeFailure :: ParserPrefs -> ParserInfo a -> [Context] -> ParseError -> ParserResult b
makeFailure parser_prefs parser_info contexts err =
    Failure (parserFailure parser_prefs parser_info err contexts)

-- Construct a delayed failure because the executable name is available only when the result is handled.
parserFailure :: ParserPrefs -> ParserInfo a -> ParseError -> [Context] -> ParserFailure ParserHelp
parserFailure parser_prefs parser_info err contexts = ParserFailure (\program_name ->
    ( ParserHelp (render_with_context program_name)
    , errorExitCode parser_info err
    , prefColumns parser_prefs
    )) where
        render_with_context program_name = case contexts of
            [] -> renderParserMessage parser_prefs parser_info program_name err
            Context _ command_info:_ -> renderParserMessage parser_prefs command_info
                (unwords (program_name:contextNames contexts)) err

renderFailure :: ParserFailure ParserHelp -> String -> (String, ExitCode)
renderFailure (ParserFailure render) program_name = case render program_name of
    (ParserHelp message, exit_code, _) -> (message, exit_code)

-- Transform generated help without changing the exit status or configured width.
overFailure :: (ParserHelp -> ParserHelp) -> ParserResult a -> ParserResult a
overFailure transform (Failure (ParserFailure render)) = Failure (ParserFailure (\program_name ->
    case render program_name of
        (parser_help, exit_code, width) -> (transform parser_help, exit_code, width)))
overFailure _ result = result

execParser :: ParserInfo a -> IO a
execParser = customExecParser defaultPrefs

-- Parse the process arguments and delegate all terminal output and controlled exit behavior.
customExecParser :: ParserPrefs -> ParserInfo a -> IO a
customExecParser parser_prefs parser_info = do
    arguments <- getArgs
    handleParseResult (execParserPure parser_prefs parser_info arguments)

-- Print informational exits to stdout, errors to stderr, and let the top-level runner perform cleanup.
handleParseResult :: ParserResult a -> IO a
handleParseResult (Success x) = return x
handleParseResult (Failure failure) = do
    program_name <- getProgName
    let (message, exit_code) = renderFailure failure program_name
    case exit_code of
        ExitSuccess -> putStrLn message
        ExitFailure _ -> hPutStrLn stderr message
    exitWith exit_code

errorExitCode :: ParserInfo a -> ParseError -> ExitCode
errorExitCode _ (ShowHelpText _) = ExitSuccess
errorExitCode _ (InfoMsg _) = ExitSuccess
errorExitCode parser_info _ = ExitFailure (infoFailureCode parser_info)
