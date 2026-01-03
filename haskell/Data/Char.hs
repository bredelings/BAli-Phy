{-# LANGUAGE NoImplicitPrelude #-}
module Data.Char where

-- Current Char is just a char, but it probably should be a unicode code point (32-bits).

import Compiler.Error
import Compiler.Base
import Compiler.Num
import Data.Eq
import Data.Ord

foreign import ecall "Char:isControl" builtin_isControl :: Char -> Int

isControl :: Char -> Bool
isControl c = if builtin_isControl c == 1 then True else False

foreign import ecall "Char:isSpace" builtin_isSpace :: Char -> Int

isSpace :: Char -> Bool
isSpace c = if builtin_isSpace c == 1 then True else False

foreign import ecall "Char:isLower" builtin_isLower :: Char -> Int

isLower :: Char -> Bool
isLower c = if builtin_isLower c == 1 then True else False

foreign import ecall "Char:isUpper" builtin_isUpper :: Char -> Int

isUpper :: Char -> Bool
isUpper c = if builtin_isUpper c == 1 then True else False

foreign import ecall "Char:isAlpha" builtin_isAlpha :: Char -> Int

isAlpha :: Char -> Bool
isAlpha c = if builtin_isAlpha c == 1 then True else False

foreign import ecall "Char:isAlphaNum" builtin_isAlphaNum :: Char -> Int

isAlphaNum :: Char -> Bool
isAlphaNum c = if builtin_isAlphaNum c == 1 then True else False

foreign import ecall "Char:isPrint" builtin_isPrint :: Char -> Int

isPrint :: Char -> Bool
isPrint c = if builtin_isPrint c == 1 then True else False

foreign import ecall "Char:isDigit" builtin_isDigit :: Char -> Int

isDigit :: Char -> Bool
isDigit c = if builtin_isDigit c == 1 then True else False

-- isOctDigit :: Char -> Bool
-- not implemented

foreign import ecall "Char:isHexDigit" builtin_isHexDigit :: Char -> Int

isHexDigit :: Char -> Bool
isHexDigit c = if builtin_isHexDigit c == 1 then True else False

isLetter :: Char -> Bool
isLetter = isAlpha

-- isMark :: Char -> Bool
-- not implemented

-- isNumber :: Char -> Bool
-- not implemented

foreign import ecall "Char:isPunctuation" builtin_isPunctuation :: Char -> Int

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
foreign import ecall "Char:toUpper" toUpper :: Char -> Char

foreign import ecall "Char:toLower" toLower :: Char -> Char

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

foreign import ecall "Char:intToDigit" intToDigit :: Int -> Char

-- numeric representations
foreign import ecall "Char:ord" ord :: Char -> Int

foreign import ecall "Char:chr" chr :: Int -> Char

-- string representations
-- showLitChar :: Char -> ShowS
-- not implemented

-- lexLitChar :: ReadS String
-- not implemented

-- readLitChar :: ReadS Char
-- not implemented
