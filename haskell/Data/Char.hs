{-# LANGUAGE NoImplicitPrelude #-}
module Data.Char where

import Compiler.Base
import Compiler.Num
import Data.Eq
import Data.Ord

data Char

builtin_isControl :: Char -> Int
builtin builtin_isControl 1 "isControl" "Char"

isControl :: Char -> Bool
isControl c = if builtin_isControl c == 1 then True else False

builtin_isSpace :: Char -> Int
builtin builtin_isSpace 1 "isSpace" "Char"

isSpace :: Char -> Bool
isSpace c = if builtin_isSpace c == 1 then True else False

builtin_isLower :: Char -> Int
builtin builtin_isLower 1 "isLower" "Char"

isLower :: Char -> Bool
isLower c = if builtin_isLower c == 1 then True else False

builtin_isUpper :: Char -> Int
builtin builtin_isUpper 1 "isUpper" "Char"

isUpper :: Char -> Bool
isUpper c = if builtin_isUpper c == 1 then True else False

builtin_isAlpha :: Char -> Int
builtin builtin_isAlpha 1 "isAlpha" "Char"

isAlpha :: Char -> Bool
isAlpha c = if builtin_isAlpha c == 1 then True else False

builtin_isAlphaNum :: Char -> Int
builtin builtin_isAlphaNum 1 "isAlphaNum" "Char"

isAlphaNum :: Char -> Bool
isAlphaNum c = if builtin_isAlphaNum c == 1 then True else False

builtin_isPrint :: Char -> Int
builtin builtin_isPrint 1 "isPrint" "Char"

isPrint :: Char -> Bool
isPrint c = if builtin_isPrint c == 1 then True else False

builtin_isDigit :: Char -> Int
builtin builtin_isDigit 1 "isDigit" "Char"

isDigit :: Char -> Bool
isDigit c = if builtin_isDigit c == 1 then True else False

isOctDigit :: Char -> Bool
-- not implemented

builtin_isHexDigit :: Char -> Int
builtin builtin_isHexDigit 1 "isHexDigit" "Char"

isHexDigit :: Char -> Bool
isHexDigit c = if builtin_isHexDigit c == 1 then True else False

isLetter :: Char -> Bool
isLetter = isAlpha

isMark :: Char -> Bool
-- not implemented

isNumber :: Char -> Bool
-- not implemented

builtin_isPunctuation :: Char -> Int
builtin builtin_isPunctuation 1 "isPunctuation" "Char"

isPunctuation :: Char -> Bool
isPunctuation c = if builtin_isPunctuation c == 1 then True else False

isSymbol :: Char -> Bool
-- not implemented

isSeparator :: Char -> Bool
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
builtin toUpper 1 "toUpper" "Char"

toLower :: Char -> Char
builtin toLower 1 "toLower" "Char"

toTitle :: Char -> Char
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
builtin intToDigit 1 "intToDigit" "Char"

-- numeric representations
ord :: Char -> Int
builtin ord 1 "ord" "Char"

chr :: Int -> Char
builtin chr 1 "chr" "Char"

-- string representations
showLitChar :: Char -> ShowS
-- not implemented

lexLitChar :: ReadS String
-- not implemented

readLitChar :: ReadS Char
-- not implemented
