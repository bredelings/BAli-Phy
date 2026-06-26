module Main where

import Data.Char (chr, ord)
import System.IO (print, putStrLn)

{- Unicode in nested comments should be skipped as ordinary comment text: λ🙂 -}
main = do
  -- Unicode in line comments should also be skipped: λ🙂
  print ("lambda: λ" == "lambda: " ++ ['λ'])
  print ('λ' == chr 955)
  print (ord 'λ' == 955)
  print ('🙂' == chr 128578)
  putStrLn "λ🙂"
  print (length "λ🙂" == 2)
  putStrLn (show 'λ')
  putStrLn (show "λ🙂")
  print ("\955\&1" == "λ1")
