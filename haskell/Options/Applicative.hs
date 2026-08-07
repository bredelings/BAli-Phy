{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
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
    , mappend
    , execParserPure
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
import Compiler.Integral
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
import System.Environment
import System.Exit
import System.IO
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

data OptProperties = OptProperties
    { propertyHelp :: String
    , propertyMetavar :: String
    , propertyShowDefault :: Maybe String
    , propertyVisibility :: Visibility
    }

data OptionFields a = OptionFields [OptName] (String -> ParseError)
data FlagFields a = FlagFields [OptName]
data ArgumentFields a = ArgumentFields
data CommandFields a = CommandFields [(String, ParserInfo a)]

class HasName f where
    addName :: OptName -> f a -> f a

instance HasName OptionFields where
    addName name (OptionFields names no_arg_error) = OptionFields (name:names) no_arg_error

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

data DefaultProp a = DefaultProp (Maybe a) (Maybe (a -> String))

instance Semigroup (DefaultProp a) where
    DefaultProp value1 show1 <> DefaultProp value2 show2 =
        DefaultProp (value1 <|> value2) (show1 <|> show2)

instance Monoid (DefaultProp a) where
    mempty = DefaultProp Nothing Nothing

-- Keep typed defaults outside OptProperties so parser structure determines whether a field is required.
data Mod f a = Mod (f a -> f a) (DefaultProp a) (OptProperties -> OptProperties)

instance Semigroup (Mod f a) where
    Mod fields1 defaults1 props1 <> Mod fields2 defaults2 props2 =
        Mod (fields2 . fields1) (defaults2 <> defaults1) (props2 . props1)

instance Monoid (Mod f a) where
    mempty = Mod id mempty id

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f mempty id

defaultMod :: DefaultProp a -> Mod f a
defaultMod defaults = Mod id defaults id

propertyMod :: (OptProperties -> OptProperties) -> Mod f a
propertyMod f = Mod id mempty f

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

data OptReader a
    = OptReader [OptName] (ReadM a) (String -> ParseError)
    | FlagReader [OptName] a
    | ArgReader (ReadM a)
    | CmdReader [(String, ParserInfo a)]

data Option a = Option (OptReader a) OptProperties

-- Retain the parser's applicative structure so each command-line word can search every parser leaf.
data Parser a
    = NilP (Maybe a)
    | OptP (Option a)
    | forall x. MultP (Parser (x -> a)) (Parser x)
    | AltP (Parser a) (Parser a)
    | forall x. BindP (Parser x) (x -> Parser a)

instance Functor Option where
    fmap f (Option reader properties) = Option (fmap f reader) properties

instance Functor OptReader where
    fmap f (OptReader names reader no_arg_error) = OptReader names (fmap f reader) no_arg_error
    fmap f (FlagReader names x) = FlagReader names (f x)
    fmap f (ArgReader reader) = ArgReader (fmap f reader)
    fmap f (CmdReader commands) = CmdReader [(name, fmap f parser_info) | (name, parser_info) <- commands]

instance Functor Parser where
    fmap f (NilP value') = NilP (fmap f value')
    fmap f (OptP option') = OptP (fmap f option')
    fmap f (MultP parser_f parser_x) = MultP (fmap (f .) parser_f) parser_x
    fmap f (AltP parser1 parser2) = AltP (fmap f parser1) (fmap f parser2)
    fmap f (BindP parser k) = BindP parser (fmap f . k)

instance Applicative Parser where
    pure = NilP . Just
    (<*>) = MultP

newtype ParserM r = ParserM { runParserM :: forall x. (r -> Parser x) -> Parser x }

instance Functor ParserM where
    fmap f parser_m = parser_m >>= pure . f

instance Applicative ParserM where
    pure x = ParserM (\k -> k x)
    parser_f <*> parser_x = parser_f >>= (\f -> fmap f parser_x)

instance Monad ParserM where
    ParserM parser >>= f = ParserM (\k -> parser (\x -> runParserM (f x) k))

fromM :: ParserM a -> Parser a
fromM (ParserM parser) = parser pure

oneM :: Parser a -> ParserM a
oneM parser = ParserM (BindP parser)

-- Express repetition through BindP, matching upstream's parser representation and consumption model.
manyM :: Parser a -> ParserM [a]
manyM parser = do
    next <- oneM (optional parser)
    case next of
        Nothing -> pure []
        Just x -> fmap ((:) x) (manyM parser)

someM :: Parser a -> ParserM [a]
someM parser = liftA2 (:) (oneM parser) (manyM parser)

instance Alternative Parser where
    empty = NilP Nothing
    (<|>) = AltP
    many = fromM . manyM
    some = fromM . someM

baseProperties :: String -> OptProperties
baseProperties metavar_name = OptProperties "" metavar_name Nothing Visible

applyMod :: Mod f a -> f a -> OptProperties -> (f a, DefaultProp a, OptProperties)
applyMod (Mod modify_fields defaults modify_properties) fields properties =
    (modify_fields fields, defaults, modify_properties properties)

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

data ArgPolicy = Intersperse | NoIntersperse | AllPositionals | ForwardOptions
    deriving (Eq, Show)

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
    { prefShowHelpOnError :: Bool
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

data OptWord = OptWord OptName (Maybe String)

data LeafReply a
    = LeafNoMatch
    | LeafFailed ParseError
    | LeafMatched a [String]

data SearchReply a
    = SearchNoMatch
    | SearchFailed ParseError
    | SearchMatched (Parser a) [String]

data ParseReply a
    = Parsed a [String]
    | ParseFailed ParseError

parseWord :: String -> Maybe OptWord
-- Split only the current option word; a following value remains raw until a matching option consumes it.
parseWord ('-':'-':body) = case break ((==) '=') body of
    (name, '=':value_text) -> Just (OptWord (OptLong name) (Just value_text))
    (name, []) -> Just (OptWord (OptLong name) Nothing)
parseWord ('-':name:characters) = Just (OptWord (OptShort name)
    (if null characters then Nothing else Just characters))
parseWord _ = Nothing

-- Search the complete parser tree and rebuild the path to the first matching leaf.
searchParser :: (forall x. Option x -> LeafReply x) -> Parser a -> SearchReply a
searchParser _ (NilP _) = SearchNoMatch
searchParser match (OptP option') = case match option' of
    LeafNoMatch -> SearchNoMatch
    LeafFailed err -> SearchFailed err
    LeafMatched x arguments -> SearchMatched (NilP (Just x)) arguments
searchParser match (MultP parser_f parser_x) = case searchParser match parser_f of
    SearchNoMatch -> case searchParser match parser_x of
        SearchNoMatch -> SearchNoMatch
        SearchFailed err -> SearchFailed err
        SearchMatched parser_x' arguments -> SearchMatched (MultP parser_f parser_x') arguments
    SearchFailed err -> SearchFailed err
    SearchMatched parser_f' arguments -> SearchMatched (MultP parser_f' parser_x) arguments
searchParser match (AltP parser1 parser2) = case searchParser match parser1 of
    SearchNoMatch -> case searchParser match parser2 of
        SearchNoMatch -> SearchNoMatch
        SearchFailed err -> SearchFailed err
        SearchMatched parser2' arguments -> SearchMatched (AltP parser1 parser2') arguments
    SearchFailed err -> SearchFailed err
    SearchMatched parser1' arguments -> SearchMatched (AltP parser1' parser2) arguments
searchParser match (BindP parser k) = case searchParser match parser of
    SearchNoMatch -> case evalParser parser of
        Nothing -> SearchNoMatch
        Just x -> searchParser match (k x)
    SearchFailed err -> SearchFailed err
    SearchMatched parser' arguments -> SearchMatched (BindP parser' k) arguments

runReader :: ReadM a -> String -> LeafReply a
runReader (ReadM reader) text = case reader text of
    Left err -> LeafFailed err
    Right x -> LeafMatched x []

showOptionName :: OptName -> String
showOptionName (OptShort name) = ['-', name]
showOptionName (OptLong name) = "--" ++ name

-- Match one named option and consume its payload only after its parser leaf has matched.
matchOption :: OptWord -> [String] -> Option a -> LeafReply a
matchOption (OptWord word_name attached) arguments (Option reader _) = case reader of
    OptReader names read_value no_arg_error -> if word_name `elem` names
        then case attached of
            Just value_text -> with_arguments arguments (runReader read_value value_text)
            Nothing -> case arguments of
                [] -> LeafFailed (no_arg_error (showOptionName word_name))
                value_text:rest -> with_arguments rest (runReader read_value value_text)
        else LeafNoMatch
    FlagReader names x -> if word_name `elem` names && flag_accepts attached word_name
        then LeafMatched x (flag_remainder attached word_name ++ arguments)
        else LeafNoMatch
    _ -> LeafNoMatch
  where
    with_arguments rest (LeafMatched x _) = LeafMatched x rest
    with_arguments _ (LeafFailed err) = LeafFailed err
    with_arguments _ LeafNoMatch = LeafNoMatch
    flag_accepts Nothing _ = True
    flag_accepts (Just _) (OptShort _) = True
    flag_accepts (Just _) (OptLong _) = False
    flag_remainder (Just characters) (OptShort _) = ['-':characters]
    flag_remainder _ _ = []

-- Match the first reachable positional or command leaf; reader failures commit that positional word.
matchArgument :: ParserPrefs -> String -> [String] -> Option a -> LeafReply a
matchArgument parser_prefs text arguments (Option reader _) = case reader of
    ArgReader read_value -> with_arguments arguments (runReader read_value text)
    CmdReader commands -> case lookup text commands of
        Nothing -> LeafNoMatch
        Just parser_info -> case runParser parser_prefs (infoPolicy parser_info) (infoParser parser_info) arguments of
            Parsed x rest -> LeafMatched x rest
            ParseFailed err -> LeafFailed err
    _ -> LeafNoMatch
  where
    with_arguments rest (LeafMatched x _) = LeafMatched x rest
    with_arguments _ (LeafFailed err) = LeafFailed err
    with_arguments _ LeafNoMatch = LeafNoMatch

stepParser :: ParserPrefs -> ArgPolicy -> String -> [String] -> Parser a -> SearchReply a
-- Interpret the current word according to policy, falling back to a positional only for ForwardOptions.
stepParser parser_prefs policy text arguments parser = case policy of
    AllPositionals -> search_argument
    ForwardOptions -> case parseWord text of
        Nothing -> search_argument
        Just word -> case searchParser (matchOption word arguments) parser of
            SearchNoMatch -> search_argument
            result -> result
    _ -> case parseWord text of
        Nothing -> search_argument
        Just word -> searchParser (matchOption word arguments) parser
  where
    search_argument = searchParser (matchArgument parser_prefs text arguments) parser

evalParser :: Parser a -> Maybe a
-- Evaluate only fully satisfied/defaulted branches; unmatched option leaves have no value.
evalParser (NilP value') = value'
evalParser (OptP _) = Nothing
evalParser (MultP parser_f parser_x) = evalParser parser_f <*> evalParser parser_x
evalParser (AltP parser1 parser2) = evalParser parser1 <|> evalParser parser2
evalParser (BindP parser k) = case evalParser parser of
    Nothing -> Nothing
    Just x -> evalParser (k x)

-- Consume argv from left to right, changing to positional-only mode only when the outer loop sees `--`.
runParser :: ParserPrefs -> ArgPolicy -> Parser a -> [String] -> ParseReply a
runParser parser_prefs policy parser ("--":arguments)
    | policy /= AllPositionals = runParser parser_prefs AllPositionals parser arguments
runParser parser_prefs policy parser arguments = case arguments of
    [] -> case evalParser parser of
        Just x -> Parsed x []
        Nothing -> ParseFailed (MissingError (missingDescription parser))
    text:rest -> case stepParser parser_prefs policy text rest parser of
        SearchFailed err -> ParseFailed err
        SearchMatched parser' remaining -> runParser parser_prefs (nextPolicy policy text) parser' remaining
        SearchNoMatch -> case evalParser parser of
            Just x -> Parsed x arguments
            Nothing -> ParseFailed (UnexpectedError text)

nextPolicy :: ArgPolicy -> String -> ArgPolicy
nextPolicy NoIntersperse text = case parseWord text of
    Nothing -> AllPositionals
    Just _ -> NoIntersperse
nextPolicy policy _ = policy

newtype ParserHelp = ParserHelp String
    deriving (Eq, Show)

newtype ParserFailure h = ParserFailure (String -> (h, ExitCode, Int))

data ParserResult a
    = Success a
    | Failure (ParserFailure ParserHelp)

instance Functor ParserResult where
    fmap f (Success x) = Success (f x)
    fmap _ (Failure failure) = Failure failure

instance Functor ParserFailure where
    fmap f (ParserFailure render) = ParserFailure (\program_name -> case render program_name of
        (parser_help, exit_code, width) -> (f parser_help, exit_code, width))

instance Applicative ParserResult where
    pure = Success
    Success f <*> result = fmap f result
    Failure failure <*> _ = Failure failure

instance Monad ParserResult where
    Success x >>= k = k x
    Failure failure >>= _ = Failure failure

instance Show h => Show (ParserFailure h) where
    show (ParserFailure render) = case render "<program>" of
        (parser_help, exit_code, width) ->
            "ParserFailure " ++ show parser_help ++ " " ++ show exit_code ++ " " ++ show width

instance Show a => Show (ParserResult a) where
    show (Success x) = "Success " ++ show x
    show (Failure failure) = "Failure (" ++ show failure ++ ")"

getParseResult :: ParserResult a -> Maybe a
getParseResult (Success x) = Just x
getParseResult _ = Nothing

-- Run a parser without terminal IO so callers can inspect success and failure directly.
execParserPure :: ParserPrefs -> ParserInfo a -> [String] -> ParserResult a
execParserPure parser_prefs parser_info arguments =
    case runParser parser_prefs (infoPolicy parser_info) (infoParser parser_info) arguments of
        Parsed x [] -> Success x
        Parsed _ (unexpected:_) -> makeFailure parser_prefs parser_info (UnexpectedError unexpected)
        ParseFailed err -> makeFailure parser_prefs parser_info
            (if null arguments && prefShowHelpOnEmpty parser_prefs then ShowHelpText Nothing else err)

makeFailure :: ParserPrefs -> ParserInfo a -> ParseError -> ParserResult b
makeFailure parser_prefs parser_info err = Failure (parserFailure parser_prefs parser_info err)

-- Construct a delayed failure because the executable name is available only when the result is handled.
parserFailure :: ParserPrefs -> ParserInfo a -> ParseError -> ParserFailure ParserHelp
parserFailure parser_prefs parser_info err = ParserFailure (\program_name ->
    ( ParserHelp (renderParserMessage parser_prefs parser_info program_name err)
    , errorExitCode parser_info err
    , prefColumns parser_prefs
    ))

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

-- Select an informational message, full help, or a concise error with optional help.
renderParserMessage :: ParserPrefs -> ParserInfo a -> String -> ParseError -> String
renderParserMessage parser_prefs parser_info program_name err = case err of
    InfoMsg message -> message
    ShowHelpText _ -> renderHelp parser_prefs parser_info program_name (infoFullDesc parser_info)
    _ -> renderError err ++ if prefShowHelpOnError parser_prefs
        then "\n\n" ++ renderHelp parser_prefs parser_info program_name (infoFullDesc parser_info)
        else "\n\n" ++ renderUsage parser_info program_name

renderError :: ParseError -> String
-- Translate the local structured errors into stable, user-facing one-line messages.
renderError (ErrorMsg message) = "Error: " ++ message
renderError (MissingError item) = "Missing: " ++ item
renderError (ExpectsArgError item) = "Option requires an argument: " ++ item
renderError (UnexpectedError item) = "Invalid option or argument: " ++ item
renderError UnknownError = "Invalid command line"
renderError (InfoMsg message) = message
renderError (ShowHelpText _) = ""

data OptTree a
    = Leaf a
    | MultNode [OptTree a]
    | AltNode Bool [OptTree a]
    | BindNode (OptTree a)

-- Traverse the parser structure while retaining alternatives and repetition for usage rendering.
treeMapParser :: (forall x. Option x -> a) -> Parser b -> OptTree a
treeMapParser _ (NilP _) = MultNode []
treeMapParser f (OptP option') = Leaf (f option')
treeMapParser f (MultP parser1 parser2) = MultNode [treeMapParser f parser1, treeMapParser f parser2]
treeMapParser f (AltP parser1 parser2) = AltNode
    (isJust (evalParser parser1) || isJust (evalParser parser2))
    [treeMapParser f parser1, treeMapParser f parser2]
treeMapParser f (BindP parser k) = BindNode (case evalParser parser of
    Nothing -> treeMapParser f parser
    Just x -> MultNode [treeMapParser f parser, treeMapParser f (k x)])

mapParser :: (forall x. Option x -> a) -> Parser b -> [a]
mapParser f = flatten . treeMapParser f where
    flatten (Leaf x) = [x]
    flatten (MultNode trees) = concatMap flatten trees
    flatten (AltNode _ trees) = concatMap flatten trees
    flatten (BindNode tree) = flatten tree

filterOptional :: OptTree a -> OptTree a
filterOptional (Leaf x) = Leaf x
filterOptional (MultNode trees) = MultNode (map filterOptional trees)
filterOptional (AltNode True _) = MultNode []
filterOptional (AltNode False trees) = AltNode False (map filterOptional trees)
filterOptional (BindNode tree) = BindNode (filterOptional tree)

optionUsageLabel :: Option a -> String
optionUsageLabel (Option reader properties)
    | propertyVisibility properties /= Visible = ""
    | otherwise = case reader of
        OptReader names _ _ -> describeOption names (propertyMetavar properties)
        FlagReader names _ -> describeOption names ""
        ArgReader _ -> propertyMetavar properties
        CmdReader _ -> propertyMetavar properties

-- Render required products, alternatives, defaults, and repetitions from parser structure.
renderUsageTree :: OptTree String -> String
renderUsageTree (Leaf text) = text
renderUsageTree (MultNode trees) = unwords (filter (not . null) (map renderUsageTree trees))
renderUsageTree (AltNode optional_branch trees) = case filter (not . null) (map renderUsageTree trees) of
    [] -> ""
    [text] -> if optional_branch then "[" ++ text ++ "]" else text
    alternatives -> (if optional_branch then "[" else "(")
        ++ intercalate "|" alternatives ++ if optional_branch then "]" else ")"
renderUsageTree (BindNode tree) = case renderUsageTree tree of
    "" -> ""
    text -> text ++ "..."

missingDescription :: Parser a -> String
missingDescription parser = case renderUsageTree (filterOptional (treeMapParser optionUsageLabel parser)) of
    "" -> "required option or argument"
    text -> text

describeOption :: [OptName] -> String -> String
-- Render a compact name used in diagnostics and usage text.
describeOption [] metavar_name = metavar_name
describeOption names metavar_name = intercalate "/" (map showOptionName (reverse names)) ++ suffix where
    suffix = if null metavar_name then "" else " " ++ metavar_name

-- Render plain help using only strings, with the configured width controlling description wrapping.
renderHelp :: ParserPrefs -> ParserInfo a -> String -> Bool -> String
renderHelp parser_prefs parser_info program_name show_full = intercalate "\n\n" (filter (not . null)
    [ infoHeader parser_info
    , renderUsage parser_info program_name
    , infoProgDesc parser_info
    , if show_full then renderEntryGroups (prefColumns parser_prefs) (infoParser parser_info) else ""
    , infoFooter parser_info
    ])

renderUsage :: ParserInfo a -> String -> String
-- Form the compact synopsis from the parser tree so brackets reflect structural defaults.
renderUsage parser_info program_name = "Usage: " ++ program_name ++ usage_suffix where
    usage_text = renderUsageTree (treeMapParser optionUsageLabel (infoParser parser_info))
    usage_suffix = if null usage_text then "" else " " ++ usage_text

data EntryKind = OptionEntry | CommandEntry
    deriving (Eq)

-- Convert parser leaves to display rows without using those rows to control parsing or requiredness.
descriptionRows :: Parser a -> [(EntryKind, String, String)]
descriptionRows parser = concat (mapParser rows parser) where
    rows (Option reader properties)
        | propertyVisibility properties == Internal = []
        | otherwise = case reader of
            CmdReader commands -> [(CommandEntry, name, infoProgDesc parser_info)
                                  | (name, parser_info) <- reverse commands]
            _ -> [(OptionEntry, option_label reader properties, option_description properties)]
    option_label reader properties = case reader of
        OptReader names _ _ -> describeOption names (propertyMetavar properties)
        FlagReader names _ -> describeOption names ""
        ArgReader _ -> propertyMetavar properties
        CmdReader _ -> propertyMetavar properties
    option_description properties = add_default (propertyHelp properties) (propertyShowDefault properties)

-- Separate ordinary options and commands while retaining declaration order within each group.
renderEntryGroups :: Int -> Parser a -> String
renderEntryGroups width parser = intercalate "\n\n" (filter (not . null)
    [ renderGroup "Available options:" width [(label, description)
        | (OptionEntry, label, description) <- rows]
    , renderGroup "Available commands:" width [(label, description)
        | (CommandEntry, label, description) <- rows]
    ]) where
        rows = descriptionRows parser

renderGroup :: String -> Int -> [(String, String)] -> String
renderGroup title width entries = case concatMap (renderEntry width) entries of
    [] -> ""
    rows -> title ++ "\n" ++ unlines rows

-- Align descriptions at a fixed column and wrap continuation lines within the requested width.
renderEntry :: Int -> (String, String) -> [String]
renderEntry width (label, description) = case wrapWords description_width description of
    [] -> ["  " ++ label]
    first_line:rest -> (padRight label_width ("  " ++ label) ++ first_line)
        : map ((++) (replicate label_width ' ')) rest
  where
    label_width = min 28 (max 12 (width `div` 3))
    description_width = max 12 (width - label_width)

add_default :: String -> Maybe String -> String
add_default description Nothing = description
add_default description (Just default_value) = description ++ prefix ++ "default: " ++ default_value ++ ")"
  where
    prefix = if null description then "(" else " ("

-- Greedily wrap words without splitting individual words.
wrapWords :: Int -> String -> [String]
wrapWords width text = finish (foldl add_word ([], "") (words text)) where
    add_word (completed, "") word = (completed, word)
    add_word (completed, current) word
        | length current + 1 + length word <= width = (completed, current ++ " " ++ word)
        | otherwise = (completed ++ [current], word)
    finish (completed, "") = completed
    finish (completed, current) = completed ++ [current]

padRight :: Int -> String -> String
padRight width text = text ++ replicate (max 1 (width - length text)) ' '
