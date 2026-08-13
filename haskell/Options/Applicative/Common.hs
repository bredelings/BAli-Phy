{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Common
    ( Parser
    , liftOpt
    , showOption
    , optionNames
    , evalParser
    , runParser
    , runParserInfo
    , runParserFully
    , runParserStep
    , treeMapParser
    , mapParser
    , filterOptional
    , optionUsageLabel
    , renderUsageTree
    , missingDescription
    , describeOption
    , showOptionName
    ) where

import Compiler.Base
import Compiler.Classes
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Options.Applicative.Internal
import Options.Applicative.Types

-- Compatibility note: this follows upstream's ordinary nondeterministic parse
-- path, but abbreviations, configurable subparser backtracking, completion, and
-- ArgumentReachability-aware parser traversals remain absent.
type Args = [String]

data OptWord = OptWord OptName (Maybe String)

parseWord :: String -> Maybe OptWord
-- Split only the current option word; a following value remains in the parser state.
parseWord ('-':'-':body) = case break ((==) '=') body of
    (name, '=':value_text) -> Just (OptWord (OptLong name) (Just value_text))
    (name, []) -> Just (OptWord (OptLong name) Nothing)
parseWord ('-':name:characters) = Just (OptWord (OptShort name)
    (if null characters then Nothing else Just characters))
parseWord _ = Nothing

showOptionName :: OptName -> String
showOptionName (OptShort name) = ['-', name]
showOptionName (OptLong name) = "--" ++ name

showOption :: OptName -> String
showOption = showOptionName

liftOpt :: Option a -> Parser a
liftOpt = OptP

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _ _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

-- Build the stateful action for a matching named option without consuming anything on mismatch.
optMatches :: MonadP m => OptReader a -> OptWord -> Maybe (StateT Args m a)
optMatches reader (OptWord word_name attached) = case reader of
    OptReader names read_value no_arg_error
        | word_name `elem` names -> Just (do
            arguments <- get
            case maybeToList attached ++ arguments of
                [] -> lift (errorP (no_arg_error (showOptionName word_name)))
                value_text:rest -> do
                    put rest
                    lift (runReadM (withReadM add_option_name read_value) value_text))
        | otherwise -> Nothing
    FlagReader names x
        | word_name `elem` names && flag_accepts -> Just (do
            arguments <- get
            put (flag_remainder ++ arguments)
            return x)
        | otherwise -> Nothing
    _ -> Nothing
  where
    add_option_name message = "option " ++ showOptionName word_name ++ ": " ++ message
    flag_accepts = case (word_name, attached) of
        (OptLong _, Just _) -> False
        _ -> True
    flag_remainder = case (word_name, attached) of
        (OptShort _, Just characters) -> ['-':characters]
        _ -> []

-- Search all structurally reachable leaves and return only the selected alternative branch.
searchParser :: Monad m
             => (forall x. Option x -> NondetT m (Parser x))
             -> Parser a
             -> NondetT m (Parser a)
searchParser _ (NilP _) = empty
searchParser match (OptP option') = match option'
searchParser match (MultP parser_f parser_x) =
    (do parser_f' <- searchParser match parser_f
        return (MultP parser_f' parser_x))
    <!>
    (do parser_x' <- searchParser match parser_x
        return (MultP parser_f parser_x'))
searchParser match (AltP parser1 parser2) =
    searchParser match parser1 <|> searchParser match parser2
searchParser match (BindP parser k) =
    (do parser' <- searchParser match parser
        return (BindP parser' k))
    <|>
    case evalParser parser of
        Nothing -> empty
        Just x -> searchParser match (k x)

searchOption :: MonadP m => OptWord -> Parser a
             -> NondetT (StateT Args m) (Parser a)
searchOption word = searchParser (\(Option reader _) -> case optMatches reader word of
    Nothing -> empty
    Just matcher -> lift (fmap pure matcher))

-- Positional readers commit at the first reachable argument; commands retain their failure context.
searchArgument :: MonadP m => String -> Parser a
               -> NondetT (StateT Args m) (Parser a)
searchArgument text = searchParser (\(Option reader _) -> case reader of
    ArgReader read_value -> do
        cut
        fmap pure (lift (lift (runReadM read_value text)))
    CmdReader commands -> do
        parser_info <- hoistList (maybeToList (lookup text commands))
        fmap pure . lift . StateT $ \arguments ->
            enterContext text parser_info
            *> runParser (infoPolicy parser_info) CmdStart (infoParser parser_info) arguments
            <* exitContext
    _ -> empty)

stepParser :: MonadP m => ArgPolicy -> String -> Parser a
           -> NondetT (StateT Args m) (Parser a)
stepParser AllPositionals text parser = searchArgument text parser
stepParser ForwardOptions text parser = case parseWord text of
    Nothing -> searchArgument text parser
    Just word -> searchOption word parser <|> searchArgument text parser
stepParser _ text parser = case parseWord text of
    Nothing -> searchArgument text parser
    Just word -> searchOption word parser

-- Run one parser-search step with upstream's default left-biased ambiguity handling.
runParserStep :: MonadP m => ArgPolicy -> Parser a -> String -> Args
              -> m (Maybe (Parser a), Args)
runParserStep policy parser text arguments =
    runStateT (disamb True (stepParser policy text parser)) arguments

-- Consume argv left-to-right, using the selected branch as the parser for subsequent words.
runParser :: MonadP m => ArgPolicy -> IsCmdStart -> Parser a -> Args -> m (a, Args)
runParser policy _ parser ("--":arguments)
    | policy /= AllPositionals = runParser AllPositionals CmdCont parser arguments
runParser policy is_command_start parser arguments = case arguments of
    [] -> exitP is_command_start parser result
    text:rest -> do
        (next_parser, remaining) <- runParserStep policy parser text rest
        case next_parser of
            Just parser' -> runParser (nextPolicy policy text) CmdCont parser' remaining
            Nothing -> case result of
                Just value_and_rest -> return value_and_rest
                Nothing -> errorP (UnexpectedError text (SomeParser parser))
  where
    result = fmap (\x -> (x, arguments)) (evalParser parser)

nextPolicy :: ArgPolicy -> String -> ArgPolicy
nextPolicy NoIntersperse text = case parseWord text of
    Nothing -> AllPositionals
    Just _ -> NoIntersperse
nextPolicy policy _ = policy

runParserInfo :: MonadP m => ParserInfo a -> Args -> m a
runParserInfo parser_info = runParserFully (infoPolicy parser_info) (infoParser parser_info)

runParserFully :: MonadP m => ArgPolicy -> Parser a -> Args -> m a
runParserFully policy parser arguments = do
    (x, remaining) <- runParser policy CmdStart parser arguments
    case remaining of
        [] -> return x
        unexpected:_ -> errorP (UnexpectedError unexpected (SomeParser (pure ())))

evalParser :: Parser a -> Maybe a
-- Evaluate only fully satisfied/defaulted branches; unmatched option leaves have no value.
evalParser (NilP value') = value'
evalParser (OptP _) = Nothing
evalParser (MultP parser_f parser_x) = evalParser parser_f <*> evalParser parser_x
evalParser (AltP parser1 parser2) = evalParser parser1 <|> evalParser parser2
evalParser (BindP parser k) = case evalParser parser of
    Nothing -> Nothing
    Just x -> evalParser (k x)

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
describeOption names metavar_name = intercalate "/" (map showOptionName (sort names)) ++ suffix where
    suffix = if null metavar_name then "" else " " ++ metavar_name
