{-# LANGUAGE NoImplicitPrelude #-}
module Options.Applicative.Help
    ( renderParserMessage
    , renderHelp
    ) where

import Compiler.Base
import Compiler.Classes
import Compiler.Integral
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Options.Applicative.Common
import Options.Applicative.Types

-- Compatibility note: this temporary string renderer lacks upstream's Chunk,
-- Doc, prettyprinter, suggestions, global-option presentation, and styling.
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

descriptionRows :: Parser a -> [(EntryKind, String, String)]
-- Convert parser leaves to display rows without using those rows to control parsing or requiredness.
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
