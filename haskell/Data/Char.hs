{-# LANGUAGE NoImplicitPrelude #-}
module Data.Char where

import Compiler.Error
import Compiler.Base
import Compiler.Num
import Data.Bool
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

data GeneralCategory =
    UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned

foreign import ecall "Char:generalCategory" builtin_generalCategory :: Char -> Int

-- Decode the C++ category ordinal into the standard Data.Char category type.
-- The ordinal order is local to the Char builtin and mirrors these constructors.
generalCategory :: Char -> GeneralCategory
generalCategory c = case builtin_generalCategory c of
    0  -> UppercaseLetter
    1  -> LowercaseLetter
    2  -> TitlecaseLetter
    3  -> ModifierLetter
    4  -> OtherLetter
    5  -> NonSpacingMark
    6  -> SpacingCombiningMark
    7  -> EnclosingMark
    8  -> DecimalNumber
    9  -> LetterNumber
    10 -> OtherNumber
    11 -> ConnectorPunctuation
    12 -> DashPunctuation
    13 -> OpenPunctuation
    14 -> ClosePunctuation
    15 -> InitialQuote
    16 -> FinalQuote
    17 -> OtherPunctuation
    18 -> MathSymbol
    19 -> CurrencySymbol
    20 -> ModifierSymbol
    21 -> OtherSymbol
    22 -> Space
    23 -> LineSeparator
    24 -> ParagraphSeparator
    25 -> Control
    26 -> Format
    27 -> Surrogate
    28 -> PrivateUse
    29 -> NotAssigned
    _  -> error "Data.Char.generalCategory: invalid category"

-- Group the three Unicode mark categories under the standard Data.Char
-- predicate name.
isMark :: Char -> Bool
isMark c = case generalCategory c of
    NonSpacingMark -> True
    SpacingCombiningMark -> True
    EnclosingMark -> True
    _ -> False

-- Group all Unicode numeric categories.  This is broader than isDigit, which
-- follows Haskell and accepts only ASCII decimal digits.
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
    DecimalNumber -> True
    LetterNumber -> True
    OtherNumber -> True
    _ -> False

foreign import ecall "Char:isPunctuation" builtin_isPunctuation :: Char -> Int

isPunctuation :: Char -> Bool
isPunctuation c = if builtin_isPunctuation c == 1 then True else False

-- Group the four Unicode symbol categories under the standard Data.Char
-- predicate name.
isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    _ -> False

-- Group the Unicode separator categories under the standard Data.Char
-- predicate name.
isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
    Space -> True
    LineSeparator -> True
    ParagraphSeparator -> True
    _ -> False

-- subranges
isAscii :: Char -> Bool
isAscii c = ord c <= 127

isLatin1 :: Char -> Bool
isLatin1 c = ord c <= 255

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower :: Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

-- case conversion
foreign import ecall "Char:toUpper" toUpper :: Char -> Char

foreign import ecall "Char:toLower" toLower :: Char -> Char

foreign import ecall "Char:toTitle" toTitle :: Char -> Char

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
