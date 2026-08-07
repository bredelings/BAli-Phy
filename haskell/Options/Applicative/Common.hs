{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Common
    ( evalParser
    , runParser
    , ParseReply(..)
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
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Options.Applicative.Types

-- Compatibility note: upstream Common uses nondeterministic search, positional
-- cuts, disambiguation, and configurable subparser backtracking still to be ported.
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
describeOption names metavar_name = intercalate "/" (map showOptionName (reverse names)) ++ suffix where
    suffix = if null metavar_name then "" else " " ++ metavar_name
