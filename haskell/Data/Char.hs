{-# LANGUAGE NoImplicitPrelude #-}
module Data.Char where

-- Current Char is just a char, but it probably should be a unicode code point (32-bits).

import Compiler.Base
import Compiler.Num
import Data.Eq
import Data.Ord

builtin_isControl :: Char -> Int
builtin builtin_isControl 1 "Char:isControl"

isControl :: Char -> Bool
isControl c = if builtin_isControl c == 1 then True else False

builtin_isSpace :: Char -> Int
builtin builtin_isSpace 1 "Char:isSpace"

isSpace :: Char -> Bool
isSpace c = if builtin_isSpace c == 1 then True else False

builtin_isLower :: Char -> Int
builtin builtin_isLower 1 "Char:isLower"

isLower :: Char -> Bool
isLower c = if builtin_isLower c == 1 then True else False

builtin_isUpper :: Char -> Int
builtin builtin_isUpper 1 "Char:isUpper"

isUpper :: Char -> Bool
isUpper c = if builtin_isUpper c == 1 then True else False

builtin_isAlpha :: Char -> Int
builtin builtin_isAlpha 1 "Char:isAlpha"

isAlpha :: Char -> Bool
isAlpha c = if builtin_isAlpha c == 1 then True else False

builtin_isAlphaNum :: Char -> Int
builtin builtin_isAlphaNum 1 "Char:isAlphaNum"

isAlphaNum :: Char -> Bool
isAlphaNum c = if builtin_isAlphaNum c == 1 then True else False

builtin_isPrint :: Char -> Int
builtin builtin_isPrint 1 "Char:isPrint"

isPrint :: Char -> Bool
isPrint c = if builtin_isPrint c == 1 then True else False

builtin_isDigit :: Char -> Int
builtin builtin_isDigit 1 "Char:isDigit"

isDigit :: Char -> Bool
isDigit c = if builtin_isDigit c == 1 then True else False

-- isOctDigit :: Char -> Bool
-- not implemented

builtin_isHexDigit :: Char -> Int
builtin builtin_isHexDigit 1 "Char:isHexDigit"

isHexDigit :: Char -> Bool
isHexDigit c = if builtin_isHexDigit c == 1 then True else False

isLetter :: Char -> Bool
isLetter = isAlpha

-- isMark :: Char -> Bool
-- not implemented

-- isNumber :: Char -> Bool
-- not implemented

builtin_isPunctuation :: Char -> Int
builtin builtin_isPunctuation 1 "Char:isPunctuation"

isPunctuation :: Char -> Bool
isPunctuation c = if builtin_isPunctuation c == 1 then True else False

-- isSymbol :: Char -> Bool
-- not implemented

-- isSeparator :: Char -> Bool
-- not implemented

-- subranges
isAscii :: Char -> Bool
isAscii _ = True

isLatin1 :: Char -> Bool
isLatin1 _ = True

isAsciiUpper :: Char -> Bool
isAsciiUpper = isUpper

isAsciiLower :: Char -> Bool
isAsciiLower = isLower

-- Unicode general categories
-- GeneralCategory(..), generalCategory

-- case conversion
toUpper :: Char -> Char
builtin toUpper 1 "Char:toUpper"

toLower :: Char -> Char
builtin toLower 1 "Char:toLower"

-- toTitle :: Char -> Char
-- not implemented

-- single digit characters
digitToInt :: Char -> Int
digitToInt c | dec <= 9  = dec
             | hexl <= 5 = hexl + 10
             | hexu <= 5 = hexu + 10
             | otherwise = error ("digitToInt: bad digit") --  ++ show c)
  where
    dec  = ord c - ord '0'
    hexl = ord c - ord 'a'
    hexu = ord c - ord 'A'

intToDigit :: Int -> Char
builtin intToDigit 1 "Char:intToDigit"

-- numeric representations
ord :: Char -> Int
builtin ord 1 "Char:ord"

chr :: Int -> Char
builtin chr 1 "Char:chr"

-- string representations
-- showLitChar :: Char -> ShowS
-- not implemented

-- lexLitChar :: ReadS String
-- not implemented

-- readLitChar :: ReadS Char
-- not implemented
