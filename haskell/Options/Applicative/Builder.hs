{-# LANGUAGE NoImplicitPrelude #-}
module Options.Applicative.Builder
    ( OptionFields
    , FlagFields
    , ArgumentFields
    , CommandFields
    , HasName
    , HasValue
    , HasMetavar
    , Mod
    , flag
    , flag'
    , switch
    , option
    , strOption
    , argument
    , strArgument
    , subparser
    , abortOption
    , infoOption
    , helper
    , simpleVersioner
    , auto
    , str
    , maybeReader
    , eitherReader
    , disabled
    , readerError
    , readerAbort
    , short
    , long
    , help
    , value
    , showDefaultWith
    , showDefault
    , metavar
    , hidden
    , internal
    , command
    , InfoMod
    , info
    , fullDesc
    , briefDesc
    , header
    , footer
    , progDesc
    , failureCode
    , noIntersperse
    , forwardOptions
    , PrefsMod
    , prefs
    , defaultPrefs
    , showHelpOnError
    , showHelpOnEmpty
    , columns
    , idm
    ) where

import Compiler.Base
import Compiler.Classes
import Compiler.Num
import Control.Applicative
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Options.Applicative.Builder.Internal
import Options.Applicative.Types
import Text.Read
import Text.Show

-- Compatibility note: upstream Builder also provides completers, grouping,
-- disambiguation, backtracking preferences, and richer help modifiers.
auto :: Read a => ReadM a
auto = ReadM (\text -> case [x | (x, rest) <- readsPrec 0 text, all isSpace rest] of
    (x:_) -> Right x
    [] -> Left (ErrorMsg ("cannot parse value `" ++ text ++ "'")))

str :: ReadM String
str = ReadM Right

maybeReader :: (String -> Maybe a) -> ReadM a
maybeReader reader = ReadM (\text -> case reader text of
    Just x -> Right x
    Nothing -> Left (ErrorMsg ("cannot parse value `" ++ text ++ "'")))

eitherReader :: (String -> Either String a) -> ReadM a
eitherReader reader = ReadM (\text -> case reader text of
    Left message -> Left (ErrorMsg message)
    Right x -> Right x)

disabled :: ReadM a
disabled = readerError "disabled option"

short :: HasName f => Char -> Mod f a
short = fieldMod . addName . OptShort

long :: HasName f => String -> Mod f a
long = fieldMod . addName . OptLong

help :: String -> Mod f a
help text = propertyMod (\properties -> properties { propertyHelp = text })

value :: HasValue f => a -> Mod f a
value x = defaultMod (DefaultProp (Just x) Nothing)

showDefaultWith :: (a -> String) -> Mod f a
showDefaultWith show_value = defaultMod (DefaultProp Nothing (Just show_value))

showDefault :: Show a => Mod f a
showDefault = showDefaultWith show

metavar :: HasMetavar f => String -> Mod f a
metavar name = propertyMod (\properties -> properties { propertyMetavar = name })

hidden :: Mod f a
hidden = propertyMod (\properties -> properties { propertyVisibility = Hidden })

internal :: Mod f a
internal = propertyMod (\properties -> properties { propertyVisibility = Internal })

command :: String -> ParserInfo a -> Mod CommandFields a
command name parser_info = fieldMod add_command where
    add_command (CommandFields commands) = CommandFields ((name, parser_info):commands)

baseProperties :: String -> OptProperties
baseProperties metavar_name = OptProperties "" metavar_name Nothing Visible

-- Store a default in an alternative pure branch and only store its rendered text in the option leaf.
makeParser :: DefaultProp a -> OptProperties -> OptReader a -> Parser a
makeParser (DefaultProp default_value show_default) properties reader = case default_value of
    Nothing -> option_parser
    Just x -> option_parser <|> pure x
  where
    option_parser = OptP (Option reader properties
        { propertyShowDefault = case (default_value, show_default) of
            (Just x, Just render) -> Just (render x)
            _ -> Nothing
        })

option :: ReadM a -> Mod OptionFields a -> Parser a
-- Build a named option whose reader consumes an attached or immediately following raw word.
option reader modifiers = makeParser defaults properties (OptReader names reader no_arg_error) where
    (OptionFields names no_arg_error, defaults, properties) = applyMod modifiers
        (OptionFields [] ExpectsArgError) (baseProperties "ARG")

strOption :: Mod OptionFields String -> Parser String
strOption = option str

argument :: ReadM a -> Mod ArgumentFields a -> Parser a
-- Build a positional leaf; the word-driven runner decides when it is reachable under the active policy.
argument reader modifiers = makeParser defaults properties (ArgReader reader) where
    (_, defaults, properties) = applyMod modifiers ArgumentFields (baseProperties "ARG")

strArgument :: Mod ArgumentFields String -> Parser String
strArgument = argument str

flag' :: a -> Mod FlagFields a -> Parser a
-- Build a flag leaf that succeeds only when one of its names matches the current option word.
flag' active modifiers = makeParser defaults properties (FlagReader names active) where
    (FlagFields names, defaults, properties) = applyMod modifiers (FlagFields []) (baseProperties "")

flag :: a -> a -> Mod FlagFields a -> Parser a
flag default_value active modifiers = flag' active modifiers <|> pure default_value

switch :: Mod FlagFields Bool -> Parser Bool
switch = flag False True

subparser :: Mod CommandFields a -> Parser a
-- Build one command leaf; selecting a command runs its ParserInfo over the remaining raw arguments.
subparser modifiers = makeParser defaults properties (CmdReader commands) where
    (CommandFields commands, defaults, properties) =
        applyMod modifiers (CommandFields []) (baseProperties "COMMAND")

-- Build an informational option as an ordinary defaulted option whose matched reader aborts parsing.
abortOption :: ParseError -> Mod OptionFields (a -> a) -> Parser (a -> a)
abortOption err modifiers = makeParser defaults properties (OptReader names (readerAbort err) (const err)) where
    (OptionFields names _, defaults, properties) = applyMod
        (value id <> metavar "" <> modifiers) (OptionFields [] ExpectsArgError) (baseProperties "ARG")

infoOption :: String -> Mod OptionFields (a -> a) -> Parser (a -> a)
infoOption = abortOption . InfoMsg

helper :: Parser (a -> a)
helper = abortOption (ShowHelpText Nothing)
    (long "help" <> short 'h' <> help "Show this help text")

simpleVersioner :: String -> Parser (a -> a)
simpleVersioner version = infoOption version
    (long "version" <> help "Show version information" <> hidden)

newtype InfoMod a = InfoMod (ParserInfo a -> ParserInfo a)

instance Semigroup (InfoMod a) where
    InfoMod first <> InfoMod second = InfoMod (second . first)

instance Monoid (InfoMod a) where
    mempty = InfoMod id

info :: Parser a -> InfoMod a -> ParserInfo a
info parser (InfoMod modify) = modify (ParserInfo parser True "" "" "" 1 Intersperse)

fullDesc :: InfoMod a
fullDesc = InfoMod (\parser_info -> parser_info { infoFullDesc = True })

briefDesc :: InfoMod a
briefDesc = InfoMod (\parser_info -> parser_info { infoFullDesc = False })

header :: String -> InfoMod a
header text = InfoMod (\parser_info -> parser_info { infoHeader = text })

footer :: String -> InfoMod a
footer text = InfoMod (\parser_info -> parser_info { infoFooter = text })

progDesc :: String -> InfoMod a
progDesc text = InfoMod (\parser_info -> parser_info { infoProgDesc = text })

failureCode :: Int -> InfoMod a
failureCode code = InfoMod (\parser_info -> parser_info { infoFailureCode = code })

noIntersperse :: InfoMod a
noIntersperse = InfoMod (\parser_info -> parser_info { infoPolicy = NoIntersperse })

forwardOptions :: InfoMod a
forwardOptions = InfoMod (\parser_info -> parser_info { infoPolicy = ForwardOptions })

newtype PrefsMod = PrefsMod (ParserPrefs -> ParserPrefs)

instance Semigroup PrefsMod where
    PrefsMod first <> PrefsMod second = PrefsMod (second . first)

instance Monoid PrefsMod where
    mempty = PrefsMod id

prefs :: PrefsMod -> ParserPrefs
prefs (PrefsMod modify) = modify (ParserPrefs False False 80)

defaultPrefs :: ParserPrefs
defaultPrefs = prefs idm

showHelpOnError :: PrefsMod
showHelpOnError = PrefsMod (\parser_prefs -> parser_prefs { prefShowHelpOnError = True })

showHelpOnEmpty :: PrefsMod
showHelpOnEmpty = PrefsMod (\parser_prefs -> parser_prefs { prefShowHelpOnEmpty = True })

columns :: Int -> PrefsMod
columns width = PrefsMod (\parser_prefs -> parser_prefs { prefColumns = width })

idm :: Monoid m => m
idm = mempty
