{-# LANGUAGE NoImplicitPrelude #-}
module Data.Char where

-- Current Char is just a char, but it probably should be a unicode code point (32-bits).

import Compiler.Base
import Compiler.Num
import Data.Eq
import Data.Ord

builtin_isControl :: Char -> Int
builtin "Char:isControl" builtin_isControl 1

isControl :: Char -> Bool
isControl c = if builtin_isControl c == 1 then True else False

builtin_isSpace :: Char -> Int
builtin "Char:isSpace" builtin_isSpace 1

isSpace :: Char -> Bool
isSpace c = if builtin_isSpace c == 1 then True else False

builtin_isLower :: Char -> Int
builtin "Char:isLower" builtin_isLower 1

isLower :: Char -> Bool
isLower c = if builtin_isLower c == 1 then True else False

builtin_isUpper :: Char -> Int
builtin "Char:isUpper" builtin_isUpper 1

isUpper :: Char -> Bool
isUpper c = if builtin_isUpper c == 1 then True else False

builtin_isAlpha :: Char -> Int
builtin "Char:isAlpha" builtin_isAlpha 1

isAlpha :: Char -> Bool
isAlpha c = if builtin_isAlpha c == 1 then True else False

builtin_isAlphaNum :: Char -> Int
builtin "Char:isAlphaNum" builtin_isAlphaNum 1

isAlphaNum :: Char -> Bool
isAlphaNum c = if builtin_isAlphaNum c == 1 then True else False

builtin_isPrint :: Char -> Int
builtin "Char:isPrint" builtin_isPrint 1

isPrint :: Char -> Bool
isPrint c = if builtin_isPrint c == 1 then True else False

builtin_isDigit :: Char -> Int
builtin "Char:isDigit" builtin_isDigit 1

isDigit :: Char -> Bool
isDigit c = if builtin_isDigit c == 1 then True else False

-- isOctDigit :: Char -> Bool
-- not implemented

builtin_isHexDigit :: Char -> Int
builtin "Char:isHexDigit" builtin_isHexDigit 1

isHexDigit :: Char -> Bool
isHexDigit c = if builtin_isHexDigit c == 1 then True else False

isLetter :: Char -> Bool
isLetter = isAlpha

-- isMark :: Char -> Bool
-- not implemented

-- isNumber :: Char -> Bool
-- not implemented

builtin_isPunctuation :: Char -> Int
builtin "Char:isPunctuation" builtin_isPunctuation 1

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
builtin "Char:toUpper" toUpper 1

toLower :: Char -> Char
builtin "Char:toLower" toLower 1

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
builtin "Char:intToDigit" intToDigit 1

-- numeric representations
ord :: Char -> Int
builtin "Char:ord" ord 1

chr :: Int -> Char
builtin "Char:chr" chr 1

-- string representations
-- showLitChar :: Char -> ShowS
-- not implemented

-- lexLitChar :: ReadS String
-- not implemented

-- readLitChar :: ReadS Char
-- not implemented
