{-# LANGUAGE NoImplicitPrelude #-}
module Options.Applicative
    ( module Control.Applicative
    , Parser
    , ReadM
    , ParserInfo(..)
    , ParserPrefs(..)
    , ParserResult(..)
    , ParserFailure
    , ParserHelp(..)
    , ParseError(..)
    , Mod
    , OptionFields
    , FlagFields
    , ArgumentFields
    , CommandFields
    , HasName
    , HasValue
    , HasMetavar
    , flag
    , flag'
    , switch
    , option
    , strOption
    , argument
    , strArgument
    , subparser
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
    , commandGroup
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
    , multiSuffix
    , disambiguate
    , showHelpOnError
    , showHelpOnEmpty
    , columns
    , idm
    , mappend
    , execParserPure
    , getParseResult
    ) where

import Compiler.Base
import Compiler.Classes
import Compiler.Num
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import System.Exit
import Text.Read
import Text.Show

data ParseError
    = ErrorMsg String
    | InfoMsg String
    | ShowHelpText (Maybe String)
    | UnknownError
    | MissingError String
    | ExpectsArgError String
    | UnexpectedError String
    deriving (Eq, Show)

newtype ReadM a = ReadM (String -> Either ParseError a)

instance Functor ReadM where
    -- Apply a function only after the underlying argument reader succeeds.
    fmap f (ReadM reader) = ReadM (map_result . reader) where
        map_result (Left err) = Left err
        map_result (Right x) = Right (f x)

instance Applicative ReadM where
    pure x = ReadM (const (Right x))
    -- Both readers inspect the same option argument, as in the upstream ReadM reader environment.
    ReadM read_f <*> ReadM read_x = ReadM (\text -> case read_f text of
        Left err -> Left err
        Right f -> case read_x text of
            Left err -> Left err
            Right x -> Right (f x))

instance Alternative ReadM where
    empty = ReadM (const (Left UnknownError))
    -- Retry the second reader on any validation failure from the first.
    ReadM first <|> ReadM second = ReadM (\text -> case first text of
        Right x -> Right x
        Left _ -> second text)

instance Monad ReadM where
    -- Select the next reader from a value while retaining the original argument text.
    ReadM reader >>= k = ReadM (\text -> case reader text of
        Left err -> Left err
        Right x -> let ReadM next = k x in next text)

instance MonadFail ReadM where
    fail = readerError

-- Parse a value only when Read consumes the complete argument.
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

readerError :: String -> ReadM a
readerError = readerAbort . ErrorMsg

readerAbort :: ParseError -> ReadM a
readerAbort err = ReadM (const (Left err))

data OptName = OptShort Char | OptLong String
    deriving (Eq, Ord, Show)

data Visibility = Visible | Hidden | Internal
    deriving (Eq, Ord, Show)

data OptProperties a = OptProperties
    { propertyHelp :: String
    , propertyMetavar :: String
    , propertyDefault :: Maybe a
    , propertyShowDefault :: Maybe (a -> String)
    , propertyVisibility :: Visibility
    }

data OptionFields a = OptionFields [OptName]
data FlagFields a = FlagFields [OptName]
data ArgumentFields a = ArgumentFields
data CommandFields a = CommandFields [(String, ParserInfo a)] (Maybe String)

class HasName f where
    addName :: OptName -> f a -> f a

instance HasName OptionFields where
    addName name (OptionFields names) = OptionFields (name:names)

instance HasName FlagFields where
    addName name (FlagFields names) = FlagFields (name:names)

class HasValue f where
    hasValueDummy :: f a -> ()

instance HasValue OptionFields where
    hasValueDummy _ = ()

instance HasValue ArgumentFields where
    hasValueDummy _ = ()

class HasMetavar f where
    hasMetavarDummy :: f a -> ()

instance HasMetavar OptionFields where
    hasMetavarDummy _ = ()

instance HasMetavar ArgumentFields where
    hasMetavarDummy _ = ()

instance HasMetavar CommandFields where
    hasMetavarDummy _ = ()

-- A modifier keeps type-specific fields separate from properties common to every parser atom.
data Mod f a = Mod (f a -> f a) (OptProperties a -> OptProperties a)

instance Semigroup (Mod f a) where
    Mod fields1 props1 <> Mod fields2 props2 = Mod (fields2 . fields1) (props2 . props1)

instance Monoid (Mod f a) where
    mempty = Mod id id

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f id

propertyMod :: (OptProperties a -> OptProperties a) -> Mod f a
propertyMod = Mod id

short :: HasName f => Char -> Mod f a
short = fieldMod . addName . OptShort

long :: HasName f => String -> Mod f a
long = fieldMod . addName . OptLong

help :: String -> Mod f a
help text = propertyMod (\properties -> properties { propertyHelp = text })

value :: HasValue f => a -> Mod f a
value x = propertyMod (\properties -> properties { propertyDefault = Just x })

showDefaultWith :: (a -> String) -> Mod f a
showDefaultWith show_value = propertyMod (\properties -> properties { propertyShowDefault = Just show_value })

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
    add_command (CommandFields commands group) = CommandFields ((name, parser_info):commands) group

commandGroup :: String -> Mod CommandFields a
commandGroup name = fieldMod set_group where
    set_group (CommandFields commands _) = CommandFields commands (Just name)

data HelpEntry
    = HelpOption [OptName] Bool String String (Maybe String) Visibility
    | HelpArgument String String (Maybe String) Visibility
    | HelpCommand String String Visibility

data ArgToken = ArgToken String Bool
    deriving (Eq, Show)

data ParseState = ParseState [ArgToken]
    deriving (Eq, Show)

data ParseReply a
    = Parsed a ParseState
    | NoMatch ParseError
    | ParseFailed ParseError

data ArgPolicy = Intersperse | NoIntersperse | ForwardOptions
    deriving (Eq, Show)

-- Each parser atom scans the remaining command line, allowing applicative fields to be reordered.
data Parser a = Parser [HelpEntry] (ArgPolicy -> ParseState -> ParseReply a)

instance Functor Parser where
    -- Transform a successful result without changing consumed arguments or help entries.
    fmap f (Parser entries parser) = Parser entries (\policy state -> case parser policy state of
        Parsed x state' -> Parsed (f x) state'
        NoMatch err -> NoMatch err
        ParseFailed err -> ParseFailed err)

instance Applicative Parser where
    pure x = Parser [] (\_ state -> Parsed x state)
    -- Run applicative components over one shared remainder while collecting both descriptions.
    Parser entries_f parse_f <*> Parser entries_x parse_x =
        Parser (entries_f ++ entries_x) (\policy state -> case parse_f policy state of
            Parsed f state' -> case parse_x policy state' of
                Parsed x state'' -> Parsed (f x) state''
                NoMatch err -> NoMatch err
                ParseFailed err -> ParseFailed err
            NoMatch err -> NoMatch err
            ParseFailed err -> ParseFailed err)

instance Alternative Parser where
    empty = Parser [] (\_ _ -> NoMatch UnknownError)
    -- Backtrack only when the first parser did not match; validation failures remain committed.
    Parser entries1 parse1 <|> Parser entries2 parse2 =
        Parser (entries1 ++ entries2) (\policy state -> case parse1 policy state of
            NoMatch _ -> parse2 policy state
            result -> result)
    many parser = repeatParser False parser
    some parser = repeatParser True parser

-- Repeat a parser until it no longer matches, rejecting a successful parser that consumes nothing.
repeatParser :: Bool -> Parser a -> Parser [a]
repeatParser require_one (Parser entries parser) = Parser entries repeat_values where
    repeat_values policy initial = loop [] initial where
        loop values state = case parser policy state of
            Parsed value state' -> if state' == state
                then ParseFailed (ErrorMsg "repeated parser accepted input without consuming an argument")
                else loop (value:values) state'
            NoMatch err -> if require_one && null values then NoMatch err else Parsed (reverse values) state
            ParseFailed err -> ParseFailed err

baseProperties :: String -> OptProperties a
baseProperties metavar_name = OptProperties "" metavar_name Nothing Nothing Visible

applyMod :: Mod f a -> f a -> OptProperties a -> (f a, OptProperties a)
applyMod (Mod modify_fields modify_properties) fields properties =
    (modify_fields fields, modify_properties properties)

defaultText :: OptProperties a -> Maybe String
defaultText properties = case (propertyDefault properties, propertyShowDefault properties) of
    (Just x, Just render) -> Just (render x)
    _ -> Nothing

-- Add a default branch only after the consuming parser has had a chance to match.
withDefault :: OptProperties a -> Parser a -> Parser a
withDefault properties parser = case propertyDefault properties of
    Nothing -> parser
    Just x -> parser <|> pure x

option :: ReadM a -> Mod OptionFields a -> Parser a
-- Build a named option that validates its attached or following argument with ReadM.
option (ReadM reader) modifiers = withDefault properties parser where
    (OptionFields names, properties) = applyMod modifiers (OptionFields []) (baseProperties "ARG")
    entry = HelpOption names True (propertyMetavar properties) (propertyHelp properties)
        (defaultText properties) (propertyVisibility properties)
    parser = Parser [entry] (\_ state -> case takeOption names state of
        OptionAbsent -> NoMatch (MissingError (describeOption names (propertyMetavar properties)))
        OptionMissingArgument name -> ParseFailed (ExpectsArgError name)
        OptionFound text state' -> case reader text of
            Left err -> ParseFailed err
            Right x -> Parsed x state')

strOption :: Mod OptionFields String -> Parser String
strOption = option str

argument :: ReadM a -> Mod ArgumentFields a -> Parser a
-- Build a positional argument that skips ordinary options under the default interspersed policy.
argument (ReadM reader) modifiers = withDefault properties parser where
    (_, properties) = applyMod modifiers ArgumentFields (baseProperties "ARG")
    entry = HelpArgument (propertyMetavar properties) (propertyHelp properties)
        (defaultText properties) (propertyVisibility properties)
    parser = Parser [entry] (\policy state -> case takeArgument policy state of
        Nothing -> NoMatch (MissingError (propertyMetavar properties))
        Just (text, state') -> case reader text of
            Left err -> ParseFailed err
            Right x -> Parsed x state')

strArgument :: Mod ArgumentFields String -> Parser String
strArgument = argument str

flag' :: a -> Mod FlagFields a -> Parser a
-- Build a flag that succeeds only when one of its names is present.
flag' active modifiers = Parser [entry] parse_flag where
    (FlagFields names, properties) = applyMod modifiers (FlagFields []) (baseProperties "")
    entry = HelpOption names False "" (propertyHelp properties) Nothing (propertyVisibility properties)
    parse_flag _ state = case takeFlag names state of
        Nothing -> NoMatch (MissingError (describeOption names ""))
        Just state' -> Parsed active state'

flag :: a -> a -> Mod FlagFields a -> Parser a
flag default_value active modifiers = flag' active modifiers <|> pure default_value

switch :: Mod FlagFields Bool -> Parser Bool
switch = flag False True

subparser :: Mod CommandFields a -> Parser a
-- Select a named command and run its independent ParserInfo over the remaining arguments.
subparser modifiers = Parser entries parse_command where
    (CommandFields commands _, properties) =
        applyMod modifiers (CommandFields [] Nothing) (baseProperties "COMMAND")
    entries = [HelpCommand name (infoProgDesc parser_info) (propertyVisibility properties)
              | (name, parser_info) <- reverse commands]
    -- A command owns the remaining arguments, while returning any unconsumed arguments to its parent.
    parse_command _ state = case takeArgument Intersperse state of
        Nothing -> NoMatch (MissingError (propertyMetavar properties))
        Just (name, state') -> case lookup name commands of
            Nothing -> ParseFailed (UnexpectedError name)
            Just parser_info -> runParserPrepared parser_info state'

data OptionTake
    = OptionAbsent
    | OptionMissingArgument String
    | OptionFound String ParseState

-- Remove the first matching flag, including one member of a grouped short-option token.
takeFlag :: [OptName] -> ParseState -> Maybe ParseState
takeFlag names (ParseState tokens) = fmap ParseState (scan tokens) where
    scan [] = Nothing
    scan (token@(ArgToken text positional):rest)
        | positional = fmap (token:) (scan rest)
        | otherwise = case matchLongFlag names text of
            True -> Just rest
            False -> case removeShortFlag names text of
                Just Nothing -> Just rest
                Just (Just replacement) -> Just (ArgToken replacement False:rest)
                Nothing -> fmap (token:) (scan rest)

matchLongFlag :: [OptName] -> String -> Bool
matchLongFlag names text = case stripPrefix "--" text of
    Just name -> OptLong name `elem` names
    Nothing -> False

-- Remove one matching character from a grouped short option and retain the other flags.
removeShortFlag :: [OptName] -> String -> Maybe (Maybe String)
removeShortFlag names ('-':c:characters)
    | c /= '-' = remove [] (c:characters)
    | otherwise = Nothing
  where
    remove _ [] = Nothing
    remove prefix (x:xs)
        | OptShort x `elem` names = case reverse prefix ++ xs of
            [] -> Just Nothing
            remaining -> Just (Just ('-':remaining))
        | otherwise = remove (x:prefix) xs
removeShortFlag _ _ = Nothing

-- Remove the first matching valued option and its attached or following argument.
takeOption :: [OptName] -> ParseState -> OptionTake
takeOption names (ParseState tokens) = scan [] tokens where
    scan _ [] = OptionAbsent
    scan prefix (token@(ArgToken text positional):rest)
        | positional = scan (token:prefix) rest
        | otherwise = case matchLongOption names text of
            Just (Just value_text) -> OptionFound value_text (ParseState (reverse prefix ++ rest))
            Just Nothing -> take_following text prefix rest
            Nothing -> case matchShortOption names text of
                Just (before, Just value_text) ->
                    OptionFound value_text (ParseState (reverse prefix ++ keep_before before rest))
                Just (before, Nothing) -> take_short_following text before prefix rest
                Nothing -> scan (token:prefix) rest
    take_following name prefix [] = OptionMissingArgument name
    take_following _ prefix (ArgToken value_text _:rest) =
        OptionFound value_text (ParseState (reverse prefix ++ rest))
    take_short_following name before prefix [] = OptionMissingArgument name
    take_short_following _ before prefix (ArgToken value_text _:rest) =
        OptionFound value_text (ParseState (reverse prefix ++ keep_before before rest))
    keep_before [] rest = rest
    keep_before before rest = ArgToken ('-':before) False:rest

matchLongOption :: [OptName] -> String -> Maybe (Maybe String)
-- Distinguish an attached long-option value from one that must come from the following token.
matchLongOption names text = case stripPrefix "--" text of
    Nothing -> Nothing
    Just body -> case break ((==) '=') body of
        (name, '=':value_text) -> if OptLong name `elem` names then Just (Just value_text) else Nothing
        (name, []) -> if OptLong name `elem` names then Just Nothing else Nothing

-- A valued short option consumes the rest of its group as its attached argument.
matchShortOption :: [OptName] -> String -> Maybe (String, Maybe String)
matchShortOption names ('-':c:characters)
    | c /= '-' = find_match [] (c:characters)
    | otherwise = Nothing
  where
    find_match _ [] = Nothing
    find_match prefix (x:xs)
        | OptShort x `elem` names = Just (reverse prefix, if null xs then Nothing else Just xs)
        | otherwise = find_match (x:prefix) xs
matchShortOption _ _ = Nothing

-- Consume the first positional token, respecting the explicit end-of-options marker.
takeArgument :: ArgPolicy -> ParseState -> Maybe (String, ParseState)
takeArgument policy (ParseState tokens) = scan [] tokens where
    scan _ [] = Nothing
    scan prefix (token@(ArgToken text positional):rest)
        | positional || not (looksLikeOption text) =
            let remaining = if policy == NoIntersperse then markPositional rest else rest
            in Just (text, ParseState (reverse prefix ++ remaining))
        | otherwise = scan (token:prefix) rest

looksLikeOption :: String -> Bool
looksLikeOption ('-':_:_) = True
looksLikeOption _ = False

markPositional :: [ArgToken] -> [ArgToken]
markPositional = map mark where
    mark (ArgToken text _) = ArgToken text True

-- Normalize grouped short options before applicative branches run, so matching does not depend on
-- whether a flag parser runs before an option parser whose attached value contains that flag letter.
prepareArguments :: [HelpEntry] -> ArgPolicy -> [String] -> ParseState
prepareArguments entries policy arguments = ParseState (applyPolicy entries policy (tokenize False arguments)) where
    tokenize _ [] = []
    tokenize False ("--":rest) = tokenize True rest
    tokenize positional (text:rest)
        | positional = ArgToken text True : tokenize True rest
        | otherwise = map (\value_text -> ArgToken value_text False) (expandShortGroup entries text)
            ++ tokenize False rest

-- Re-normalize unconsumed parent tokens for a subparser while preserving `--` positional markers.
prepareExisting :: [HelpEntry] -> ArgPolicy -> ParseState -> ParseState
prepareExisting entries policy (ParseState tokens) =
    ParseState (applyPolicy entries policy (concatMap expand tokens)) where
    expand token@(ArgToken _ True) = [token]
    expand (ArgToken text False) = map (\value_text -> ArgToken value_text False) (expandShortGroup entries text)

-- Split a short group into flags until a valued option consumes the remaining characters.
expandShortGroup :: [HelpEntry] -> String -> [String]
expandShortGroup entries text@('-':c:characters)
    | c /= '-' = expand (c:characters)
    | otherwise = [text]
  where
    expand [] = []
    expand all_characters@(name:rest) = case shortOptionKind entries name of
        Just False -> ['-', name] : expand rest
        Just True -> ['-', name] : if null rest then [] else [rest]
        Nothing -> ['-':all_characters]
expandShortGroup _ text = [text]

-- Return a short option's unique argument-taking behavior; conflicting descriptions stay unsplit.
shortOptionKind :: [HelpEntry] -> Char -> Maybe Bool
shortOptionKind entries name = unique [takes_argument
    | HelpOption names takes_argument _ _ _ _ <- entries, OptShort name `elem` names] where
        unique [] = Nothing
        unique (x:xs) = if all ((==) x) xs then Just x else Nothing

-- Mark policy-dependent positional tokens once, independently of applicative parser order.
applyPolicy :: [HelpEntry] -> ArgPolicy -> [ArgToken] -> [ArgToken]
applyPolicy _ Intersperse tokens = tokens
applyPolicy entries ForwardOptions tokens = map mark_unknown tokens where
    mark_unknown token@(ArgToken text positional)
        | positional = token
        | looksLikeOption text && not (isKnownOption entries text) = ArgToken text True
        | otherwise = token
applyPolicy entries NoIntersperse tokens = stop_at_argument tokens where
    stop_at_argument [] = []
    stop_at_argument all_tokens@(token@(ArgToken text positional):rest)
        | positional = markPositional all_tokens
        | not (looksLikeOption text) = markPositional all_tokens
        | otherwise = token : if optionTakesFollowingArgument entries text
            then case rest of
                [] -> []
                value_token:remaining -> value_token : stop_at_argument remaining
            else stop_at_argument rest

isKnownOption :: [HelpEntry] -> String -> Bool
isKnownOption entries text = case optionTokenName text of
    Nothing -> False
    Just name -> any (entryHasName name) entries

entryHasName :: OptName -> HelpEntry -> Bool
entryHasName name (HelpOption names _ _ _ _ _) = name `elem` names
entryHasName _ _ = False

-- Identify exact named options that take their value from the following token.
optionTakesFollowingArgument :: [HelpEntry] -> String -> Bool
optionTakesFollowingArgument entries text = case optionTokenName text of
    Nothing -> False
    Just name -> not (hasAttachedLongValue text) && any (entryTakesArgument name) entries

entryTakesArgument :: OptName -> HelpEntry -> Bool
entryTakesArgument name (HelpOption names takes_argument _ _ _ _) = takes_argument && name `elem` names
entryTakesArgument _ _ = False

optionTokenName :: String -> Maybe OptName
optionTokenName ('-':name:[]) = Just (OptShort name)
optionTokenName text = case stripPrefix "--" text of
    Nothing -> Nothing
    Just body -> case break ((==) '=') body of
        (name, _) -> Just (OptLong name)

hasAttachedLongValue :: String -> Bool
hasAttachedLongValue text = case stripPrefix "--" text of
    Nothing -> False
    Just body -> case break ((==) '=') body of
        (_, '=':_) -> True
        _ -> False

remainingArguments :: ParseState -> [String]
remainingArguments (ParseState tokens) = [text | ArgToken text _ <- tokens]

describeOption :: [OptName] -> String -> String
-- Render a compact name used in missing-option diagnostics and usage text.
describeOption [] metavar_name = metavar_name
describeOption names metavar_name = intercalate "/" (map show_name (reverse names)) ++ suffix where
    suffix = if null metavar_name then "" else " " ++ metavar_name
    show_name (OptShort name) = ['-', name]
    show_name (OptLong name) = "--" ++ name

data ParserInfo a = ParserInfo
    { infoParser :: Parser a
    , infoFullDesc :: Bool
    , infoProgDesc :: String
    , infoHeader :: String
    , infoFooter :: String
    , infoFailureCode :: Int
    , infoPolicy :: ArgPolicy
    }

instance Functor ParserInfo where
    fmap f parser_info = parser_info { infoParser = fmap f (infoParser parser_info) }

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

data ParserPrefs = ParserPrefs
    { prefMultiSuffix :: String
    , prefDisambiguate :: Bool
    , prefShowHelpOnError :: Bool
    , prefShowHelpOnEmpty :: Bool
    , prefColumns :: Int
    }
    deriving (Eq, Show)

newtype PrefsMod = PrefsMod (ParserPrefs -> ParserPrefs)

instance Semigroup PrefsMod where
    PrefsMod first <> PrefsMod second = PrefsMod (second . first)

instance Monoid PrefsMod where
    mempty = PrefsMod id

prefs :: PrefsMod -> ParserPrefs
prefs (PrefsMod modify) = modify (ParserPrefs "" False False False 80)

defaultPrefs :: ParserPrefs
defaultPrefs = prefs idm

multiSuffix :: String -> PrefsMod
multiSuffix suffix = PrefsMod (\parser_prefs -> parser_prefs { prefMultiSuffix = suffix })

disambiguate :: PrefsMod
disambiguate = PrefsMod (\parser_prefs -> parser_prefs { prefDisambiguate = True })

showHelpOnError :: PrefsMod
showHelpOnError = PrefsMod (\parser_prefs -> parser_prefs { prefShowHelpOnError = True })

showHelpOnEmpty :: PrefsMod
showHelpOnEmpty = PrefsMod (\parser_prefs -> parser_prefs { prefShowHelpOnEmpty = True })

columns :: Int -> PrefsMod
columns width = PrefsMod (\parser_prefs -> parser_prefs { prefColumns = width })

idm :: Monoid m => m
idm = mempty

newtype ParserHelp = ParserHelp String
    deriving (Eq, Show)

newtype ParserFailure h = ParserFailure (String -> (h, ExitCode, Int))

data ParserResult a
    = Success a
    | Failure (ParserFailure ParserHelp)

instance Functor ParserResult where
    fmap f (Success x) = Success (f x)
    fmap _ (Failure failure) = Failure failure

getParseResult :: ParserResult a -> Maybe a
getParseResult (Success x) = Just x
getParseResult _ = Nothing

-- Run a parser without terminal IO so callers can inspect success and failure directly.
execParserPure :: ParserPrefs -> ParserInfo a -> [String] -> ParserResult a
execParserPure parser_prefs parser_info arguments =
    case runParserArguments parser_info arguments of
        Parsed x state -> case remainingArguments state of
            [] -> Success x
            unexpected:_ -> makeFailure parser_prefs parser_info (UnexpectedError unexpected)
        NoMatch err -> makeFailure parser_prefs parser_info err
        ParseFailed err -> makeFailure parser_prefs parser_info err

runParserState :: ParserInfo a -> ParseState -> ParseReply a
runParserState parser_info state =
    let Parser _ parser = infoParser parser_info
    in parser (infoPolicy parser_info) state

runParserArguments :: ParserInfo a -> [String] -> ParseReply a
runParserArguments parser_info arguments =
    let Parser entries _ = infoParser parser_info
    in runParserState parser_info (prepareArguments entries (infoPolicy parser_info) arguments)

-- Apply a subparser's option vocabulary and policy to the tokens its parent leaves behind.
runParserPrepared :: ParserInfo a -> ParseState -> ParseReply a
runParserPrepared parser_info state =
    let Parser entries _ = infoParser parser_info
    in runParserState parser_info (prepareExisting entries (infoPolicy parser_info) state)

makeFailure :: ParserPrefs -> ParserInfo a -> ParseError -> ParserResult b
makeFailure parser_prefs parser_info err = Failure (ParserFailure render) where
    render _ = (ParserHelp (show err), ExitFailure (infoFailureCode parser_info), prefColumns parser_prefs)
