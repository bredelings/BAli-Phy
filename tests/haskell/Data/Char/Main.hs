module Main where

import Data.Char
import System.IO (print)

main = do
  print (isAscii 'A')
  print (not (isAscii '\128'))
  print (isLatin1 '\255')
  print (not (isLatin1 '\256'))
  print (isAsciiUpper 'Z' && not (isAsciiUpper '\913'))
  print (isUpper '\913')
  print (isLower '\945')
  print (toUpper '\945' == '\913')
  print (toLower '\913' == '\945')
  print (toTitle '\945' == '\913')
  print (isDigit '9' && not (isDigit '\65296'))
  print (isAlphaNum '\65296')
  print (isSpace '\160')
  print (not (isPrint '\8232'))
  print (isPunctuation '\8212')
  print (isSymbol '\8721')
  print (isMark '\769')
  print (isSeparator '\8232')
  print (case generalCategory '\65296' of {DecimalNumber -> True; _ -> False})
  print (case generalCategory '\8212' of {DashPunctuation -> True; _ -> False})
  print ([intToDigit i | i <- [0..15]] == "0123456789abcdef")
