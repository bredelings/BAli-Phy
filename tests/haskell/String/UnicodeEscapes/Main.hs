module Main where

import System.IO

main = do
  print ("\256" == ['\256'])
  print (length "\256" == 1)
  print ("\65\&1" == "A1")
  print ("\256\&1" == ['\256', '1'])
  writeFile "unicode-string.txt" "\256"
  s <- readFile "unicode-string.txt"
  print (s == "\256")
