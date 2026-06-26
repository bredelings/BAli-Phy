{-# LANGUAGE NoImplicitPrelude #-}
module Text.Read where

import Data.Bool
import Data.Char
import Data.Eq
import Data.Maybe
import Foreign.String
import Foreign.Pair
import Data.List
import Data.Ord
import Compiler.Num
import Compiler.Error

type ReadS a = [Char] -> [(a, [Char])]

class Read a where
    readsPrec :: Int -> ReadS a
    readsPrec _ s = [(read s, [])]
    readList :: ReadS [a]
    readList = readListDefault
    read :: [Char] -> a
    read s = case [x | (x, rest) <- readsPrec 0 s, all isSpace rest] of
        (x:_) -> x
        [] -> error "Text.Read.read: no parse"

skipSpaces :: [Char] -> [Char]
skipSpaces = dropWhile isSpace

readChar :: Char -> ReadS Bool
readChar c s = case skipSpaces s of
    (x:xs) -> if x == c then [(True, xs)] else []
    [] -> []

readRawChar :: Char -> ReadS Bool
readRawChar c (x:xs) = if x == c then [(True, xs)] else []
readRawChar _ [] = []

-- Minimal token splitting for derived Read; this is not a full Haskell lexer yet.
readToken :: (Char -> Bool) -> [Char] -> ReadS Bool
readToken boundary token s = case stripPrefix token (skipSpaces s) of
    Just rest -> if tokenBoundary boundary rest then [(True, rest)] else []
    Nothing -> []

tokenBoundary :: (Char -> Bool) -> [Char] -> Bool
tokenBoundary _ [] = True
tokenBoundary boundary (c:_) = not (boundary c)

readConstructor :: [Char] -> ReadS Bool
readConstructor = readToken isIdentTail

readFieldName :: [Char] -> ReadS Bool
readFieldName = readToken isIdentTail

readInfixConstructor :: [Char] -> ReadS Bool
readInfixConstructor = readToken isSymbolChar

readPunctuation :: [Char] -> ReadS Bool
readPunctuation = readToken noTokenTail

noTokenTail :: Char -> Bool
noTokenTail _ = False

-- Match the Haskell identifier-tail categories used by haskell/ids.cc.  This
-- is still only a token-boundary helper for derived Read, not a full lexer.
isIdentTail :: Char -> Bool
isIdentTail c = isAlphaNum c || c == '_' || c == '\'' || case generalCategory c of
    NonSpacingMark -> True
    _ -> False

-- Keep the ASCII operator set explicit so quotes, underscore, and delimiters do
-- not become operators through broad Unicode punctuation categories.
isAsciiSymbolChar :: Char -> Bool
isAsciiSymbolChar c =
    c == '!' || c == '#' || c == '$' || c == '%' || c == '&' ||
    c == '*' || c == '+' || c == '.' || c == '/' || c == '<' ||
    c == '=' || c == '>' || c == '?' || c == '@' || c == '\\' ||
    c == '^' || c == '|' || c == '-' || c == '~' || c == ':'

-- Match Haskell symbolic token continuation rules without depending on the
-- lexer.  Non-ASCII punctuation uses only the operator-like category subset.
isSymbolChar :: Char -> Bool
isSymbolChar c = if isAscii c then isAsciiSymbolChar c else case generalCategory c of
    ConnectorPunctuation -> True
    DashPunctuation -> True
    OtherPunctuation -> True
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    _ -> False

readParen :: Bool -> ReadS a -> ReadS a
readParen True p s =
    [(x, s3) | (_, s1) <- readChar '(' s,
               (x, s2) <- p s1,
               (_, s3) <- readChar ')' s2]
readParen False p s = p s ++ readParen True p s

readListDefault :: Read a => ReadS [a]
readListDefault s =
    [([], s2) | (_, s1) <- readChar '[' s,
                (_, s2) <- readChar ']' s1] ++
    [(x:xs, s4) | (_, s1) <- readChar '[' s,
                  (x, s2) <- readsPrec 0 s1,
                  (xs, s4) <- readListTail s2]

readListTail :: Read a => ReadS [a]
readListTail s =
    [([], s1) | (_, s1) <- readChar ']' s] ++
    [(x:xs, s3) | (_, s1) <- readChar ',' s,
                  (x, s2) <- readsPrec 0 s1,
                  (xs, s3) <- readListTail s2]

readEscapedChar :: ReadS Char
readEscapedChar ('\\':xs) = [('\\', xs)]
readEscapedChar ('\'':xs) = [('\'', xs)]
readEscapedChar ('"':xs) = [('"', xs)]
readEscapedChar ('a':xs) = [('\a', xs)]
readEscapedChar ('b':xs) = [('\b', xs)]
readEscapedChar ('f':xs) = [('\f', xs)]
readEscapedChar ('n':xs) = [('\n', xs)]
readEscapedChar ('r':xs) = [('\r', xs)]
readEscapedChar ('t':xs) = [('\t', xs)]
readEscapedChar ('v':xs) = [('\v', xs)]
readEscapedChar s = readNamedAsciiEscape s

readNamedAsciiEscape :: ReadS Char
readNamedAsciiEscape s = [(c, rest) | (name, c) <- asciiEscapes,
                                      Just rest <- [stripPrefix name s]]

asciiEscapes :: [([Char], Char)]
asciiEscapes =
    [("NUL", chr 0), ("SOH", chr 1), ("STX", chr 2), ("ETX", chr 3),
     ("EOT", chr 4), ("ENQ", chr 5), ("ACK", chr 6), ("BEL", chr 7),
     ("BS", chr 8), ("HT", chr 9), ("LF", chr 10), ("VT", chr 11),
     ("FF", chr 12), ("CR", chr 13), ("SO", chr 14), ("SI", chr 15),
     ("DLE", chr 16), ("DC1", chr 17), ("DC2", chr 18), ("DC3", chr 19),
     ("DC4", chr 20), ("NAK", chr 21), ("SYN", chr 22), ("ETB", chr 23),
     ("CAN", chr 24), ("EM", chr 25), ("SUB", chr 26), ("ESC", chr 27),
     ("FS", chr 28), ("GS", chr 29), ("RS", chr 30), ("US", chr 31),
     ("SP", ' ')]

readLitCharUntil :: Char -> ReadS Char
readLitCharUntil _ ('\\':xs) = readEscapedChar xs
readLitCharUntil delimiter (c:xs) =
    if c == delimiter || c == '\\' || c < ' ' then [] else [(c, xs)]
readLitCharUntil _ [] = []

readCharLiteral :: ReadS Char
readCharLiteral s =
    [(c, s3) | (_, s1) <- readChar '\'' s,
               (c, s2) <- readLitCharUntil '\'' s1,
               (_, s3) <- readRawChar '\'' s2]

readStringLiteral :: ReadS [Char]
readStringLiteral s =
    [(xs, s2) | (_, s1) <- readChar '"' s,
                (xs, s2) <- readStringTail s1]

readStringTail :: ReadS [Char]
readStringTail s =
    [([], s1) | (_, s1) <- readRawChar '"' s] ++
    [(c:cs, s2) | (c, s1) <- readLitCharUntil '"' s,
                  (cs, s2) <- readStringTail s1]

instance Read Int where
    readsPrec _ s =
        let s1 = skipSpaces s
            p = read_int_at (list_to_string s1) 0
            (x, n) = pair_from_c p
        in if n < 0 then [] else [(x, drop n s1)]

instance Read Integer where
    readsPrec _ s =
        let s1 = skipSpaces s
            p = read_integer_at (list_to_string s1) 0
            (x, n) = pair_from_c p
        in if n < 0 then [] else [(x, drop n s1)]

instance Read Double where
    readsPrec _ s =
        let s1 = skipSpaces s
            p = read_double_at (list_to_string s1) 0
            (x, n) = pair_from_c p
        in if n < 0 then [] else [(x, drop n s1)]

instance Read Char where
    readsPrec _ = readCharLiteral
    readList = readStringLiteral

instance Read () where
    readsPrec _ s =
        [((), s2) | (_, s1) <- readChar '(' s,
                    (_, s2) <- readChar ')' s1]

instance (Read a, Read b) => Read (a,b) where
    readsPrec _ s =
        [((x, y), s5) | (_, s1) <- readChar '(' s,
                        (x, s2) <- readsPrec 0 s1,
                        (_, s3) <- readChar ',' s2,
                        (y, s4) <- readsPrec 0 s3,
                        (_, s5) <- readChar ')' s4]

instance (Read a, Read b, Read c) => Read (a,b,c) where
    readsPrec _ s =
        [((x, y, z), s7) | (_, s1) <- readChar '(' s,
                           (x, s2) <- readsPrec 0 s1,
                           (_, s3) <- readChar ',' s2,
                           (y, s4) <- readsPrec 0 s3,
                           (_, s5) <- readChar ',' s4,
                           (z, s6) <- readsPrec 0 s5,
                           (_, s7) <- readChar ')' s6]

instance (Read a, Read b, Read c, Read d) => Read (a,b,c,d) where
    readsPrec _ s =
        [((x, y, z, w), s9) | (_, s1) <- readChar '(' s,
                              (x, s2) <- readsPrec 0 s1,
                              (_, s3) <- readChar ',' s2,
                              (y, s4) <- readsPrec 0 s3,
                              (_, s5) <- readChar ',' s4,
                              (z, s6) <- readsPrec 0 s5,
                              (_, s7) <- readChar ',' s6,
                              (w, s8) <- readsPrec 0 s7,
                              (_, s9) <- readChar ')' s8]

instance Read a => Read [a] where
    readsPrec _ = readList

foreign import ecall "Prelude:" read_int :: CPPString -> Int
foreign import ecall "Prelude:" read_double :: CPPString -> Double
foreign import ecall "Prelude:" read_int_at :: CPPString -> Int -> EPair Int Int
foreign import ecall "Prelude:" read_integer_at :: CPPString -> Int -> EPair Integer Int
foreign import ecall "Prelude:" read_double_at :: CPPString -> Int -> EPair Double Int
