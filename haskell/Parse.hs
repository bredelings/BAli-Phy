module Parse
    ( Parser
    , runParser
    , runParserFor
    , try
    , (<?>)
    , item
    , failure
    , satisfy
    , oneOf
    , chainl
    , chainl1
    , char
    , natural
    , string
    , token
    , reserved
    , spaces
    , digit
    , number
    , parens
    , sepBy1
    , sepBy
    , option
    , optional
    , optionMaybe
    , alphaNum
    , parse_double
    ) where

import Data.Char
import Data.List (intercalate, nub)

data Input = Input Int String

data Commitment = Uncommitted | Committed

data ParseFailure = ParseFailure Int [String] Commitment

data ParseResult a = Parsed a Input | Failed ParseFailure

newtype Parser a = Parser (Input -> ParseResult a)

instance Functor Parser where
    -- Transform a successful value without changing parser state or failures.
    fmap f (Parser parser) = Parser $ \input ->
        case parser input of
          Parsed x remaining -> Parsed (f x) remaining
          Failed parseFailure -> Failed parseFailure

instance Applicative Parser where
    pure x = Parser $ \input -> Parsed x input

    -- Parse the function before its argument, committing a later failure when
    -- parsing the function consumed input.
    Parser parseFunction <*> Parser parseArgument = Parser $ \input ->
        case parseFunction input of
          Failed parseFailure -> Failed parseFailure
          Parsed function remaining ->
              case parseArgument remaining of
                Failed parseFailure -> Failed (commitAfter input remaining parseFailure)
                Parsed argument remaining' -> Parsed (function argument) remaining'

instance Alternative Parser where
    empty = failure

    -- Try the right alternative only when the left one did not commit to a
    -- recognized construct.
    Parser parser1 <|> Parser parser2 = Parser $ \input ->
        case parser1 input of
          Failed failure1@(ParseFailure _ _ Uncommitted) ->
              case parser2 input of
                Failed failure2 -> Failed (mergeAlternativeFailures failure1 failure2)
                result -> result
          result -> result
               
instance Monad Parser where
    -- Sequence parsers while committing failures after any preceding parser
    -- in the sequence has consumed input.
    Parser parser >>= function = Parser $ \input ->
        case parser input of
          Failed parseFailure -> Failed parseFailure
          Parsed value remaining ->
              case function value of
                Parser parser' ->
                    case parser' remaining of
                      Failed parseFailure -> Failed (commitAfter input remaining parseFailure)
                      Parsed value' remaining' -> Parsed value' remaining'

-- A later failure is committed when an earlier part of the same sequence
-- consumed input, even if the failing parser itself consumed nothing.
commitAfter :: Input -> Input -> ParseFailure -> ParseFailure
commitAfter (Input start _) (Input end _) (ParseFailure offset expected Uncommitted)
    | end > start = ParseFailure offset expected Committed
commitAfter _ _ parseFailure = parseFailure

-- Report the farthest alternative failure, but use the commitment of the
-- alternative actually tried last to decide whether an outer choice may run.
mergeAlternativeFailures :: ParseFailure -> ParseFailure -> ParseFailure
mergeAlternativeFailures (ParseFailure offset1 expected1 _) failure2@(ParseFailure offset2 expected2 commitment2)
    | offset1 > offset2 = ParseFailure offset1 expected1 commitment2
    | offset2 > offset1 = failure2
    | otherwise = ParseFailure offset1 (expected1 ++ expected2) commitment2

-- Allow an overlapping alternative to retry from its original input while
-- retaining the position reached by the failed parser for later diagnostics.
try :: Parser a -> Parser a
try (Parser parser) = Parser $ \input ->
    case parser input of
      Failed (ParseFailure offset expected _) -> Failed (ParseFailure offset expected Uncommitted)
      result -> result

infix 0 <?>

-- Replace an uncommitted failure's low-level expectations with a description
-- of the construct expected by its caller.
(<?>) :: Parser a -> String -> Parser a
Parser parser <?> expected = Parser $ \input ->
    case parser input of
      Failed (ParseFailure offset _ Uncommitted) -> Failed (ParseFailure offset [expected] Uncommitted)
      result -> result


-- Run a complete parser and report the farthest failure or trailing input with
-- its source position, unexpected input, and grammar expectations.
runParser :: Parser a -> String -> a
runParser = runParserFor "input"

-- Run a complete parser while identifying the kind or source of input in any
-- resulting diagnostic.
runParserFor :: String -> Parser a -> String -> a
runParserFor description (Parser parser) string =
    case parser (Input 0 string) of
      Parsed result (Input _ []) -> result
      Parsed _ (Input offset _) ->
          error (formatParseFailure description string (ParseFailure offset ["end of input"] Uncommitted))
      Failed parseFailure -> error (formatParseFailure description string parseFailure)

-- Render a parser failure after parsing has stopped, keeping source-position
-- work out of the successful parsing path.
formatParseFailure :: String -> String -> ParseFailure -> String
formatParseFailure description source (ParseFailure offset expected _) =
    "Failed to parse " ++ description ++ " at line " ++ show line ++ ", column " ++ show column ++ ":\n"
    ++ formatSourceLine line column sourceLine
    ++ "  unexpected " ++ unexpected ++ expectedLine
  where
    (line, column, sourceLine) = sourceLocation source offset
    unexpected = case drop offset source of
                   [] -> "end of input"
                   c:_ -> show c
    expectedLine = case nub expected of
                     [] -> ""
                     items -> "\n  expected " ++ formatExpected items

-- Derive the one-based location and containing line from the retained offset
-- only when a failure must be displayed.
sourceLocation :: String -> Int -> (Int, Int, String)
sourceLocation source offset =
    let prefix = take offset source
        linePrefix = reverse (takeWhile (/= '\n') (reverse prefix))
        line = 1 + length (filter (== '\n') prefix)
        column = foldl sourceColumnAfter 1 linePrefix
        sourceLine = linePrefix ++ takeWhile (/= '\n') (drop offset source)
    in (line, column, sourceLine)

-- Advance one source column, using the same eight-column tab stops as the C++
-- source diagnostic renderer.
sourceColumnAfter :: Int -> Char -> Int
sourceColumnAfter column '\t' = column + 8 - ((column - 1) `mod` 8)
sourceColumnAfter column _ = column + 1

-- Expand tabs before placing the caret so its visual position agrees with the
-- reported source column independently of the diagnostic's line-number prefix.
expandTabs :: Int -> String -> String
expandTabs _ [] = []
expandTabs column ('\t':characters) =
    let nextColumn = sourceColumnAfter column '\t'
    in replicate (nextColumn - column) ' ' ++ expandTabs nextColumn characters
expandTabs column (character:characters) = character : expandTabs (column + 1) characters

-- Display one source line and a caret using the same basic layout as
-- FileContents::print_range.
formatSourceLine :: Int -> Int -> String -> String
formatSourceLine line column sourceLine =
    replicate (lineNumberWidth + 1) ' ' ++ "|\n"
    ++ lineNumber ++ " | " ++ expandTabs 1 sourceLine ++ "\n"
    ++ replicate (lineNumberWidth + 1) ' ' ++ "| " ++ replicate (column - 1) ' ' ++ "^\n"
  where
    lineNumber = show line
    lineNumberWidth = length lineNumber

-- Join alternatives in the conventional "x, y or z" form used in parse
-- diagnostics.
formatExpected :: [String] -> String
formatExpected [] = ""
formatExpected [item] = item
formatExpected items = intercalate ", " (init items) ++ " or " ++ last items


-- Match any non-empty string, and return the first char
item :: Parser Char
item = Parser $ \(Input offset string) ->
    case string of
      [] -> Failed (ParseFailure offset ["character"] Uncommitted)
      c:cs -> Parsed c (Input (offset + 1) cs)

failure :: Parser a
failure = Parser $ \(Input offset _) -> Failed (ParseFailure offset [] Uncommitted)


-- Inspect a character without consuming it on mismatch, so alternatives can
-- distinguish an absent construct from one that began and was malformed.
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \(Input offset string) ->
    case string of
      c:cs | predicate c -> Parsed c (Input (offset + 1) cs)
      _ -> Failed (ParseFailure offset [] Uncommitted)

-- end core

oneOf s = satisfy (flip elem s) <?> "one of " ++ show s

chainl p op a = (p `chainl1` op) <|> return a

p `chainl1` op = do {a <- p; rest a}
    where rest a = (do f <- op
                       b <- p
                       rest (f a b))
                   <|> return a

char c = satisfy (c ==) <?> show c

-- where to define read_int, read_double?
natural :: Parser Int
natural = read <$> some digit

string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token p = do { a <- p; spaces ; return a}

reserved s = token (string s)

spaces = many $ satisfy isSpace

digit = satisfy isDigit <?> "digit"

number = do
  s <- string "-" <|> return []
  cs <- some digit
  return (read (s++cs) :: Int)

parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

sepBy1 p sep = do { x <- p ; rest x }
    where
      rest x = (do sep
                   xs <- sepBy1 p sep
                   return (x:xs))
               <|> return [x]

sepBy p sep = sepBy1 p sep <|> return []

option x p = p <|> return x

optional p = (p >> return ()) <|> return ()

optionMaybe p = (Just <$> p) <|> (return Nothing)

alphaNum c = satisfy isAlphaNum

-- a double is -integer[.integer][einteger]
parse_double = do s <- option [] sign
                  i1 <- some digit
                  i2 <- option [] fraction
                  i3 <- option [] exponent
                  let word = s++i1++i2++i3
                  return (read word :: Double)
    where fraction = do string "."
                        n <- some digit
                        return ('.':n)
          exponent = do string "e"
                        s <- option [] sign
                        n <- some digit
                        return ('e':(s++n))
          sign = (\c->[c]) <$> oneOf "+-"
