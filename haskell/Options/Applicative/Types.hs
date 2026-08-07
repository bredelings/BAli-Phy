{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Types
    ( ParseError(..)
    , ReadM(..)
    , readerError
    , readerAbort
    , OptName(..)
    , Visibility(..)
    , OptProperties(..)
    , OptReader(..)
    , Option(..)
    , Parser(..)
    , ParserM(..)
    , fromM
    , oneM
    , manyM
    , someM
    , ArgPolicy(..)
    , ParserInfo(..)
    , ParserPrefs(..)
    , ParserHelp(..)
    , ParserFailure(..)
    , ParserResult(..)
    , OptTree(..)
    ) where

import Compiler.Base
import Compiler.Classes
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.Exit
import Text.Show

-- Compatibility note: upstream also stores completion, backtracking, grouping,
-- and structured help data here; those fields remain to be ported.
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

data ParserPrefs = ParserPrefs
    { prefShowHelpOnError :: Bool
    , prefShowHelpOnEmpty :: Bool
    , prefColumns :: Int
    }
    deriving (Eq, Show)

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

data OptTree a
    = Leaf a
    | MultNode [OptTree a]
    | AltNode Bool [OptTree a]
    | BindNode (OptTree a)
