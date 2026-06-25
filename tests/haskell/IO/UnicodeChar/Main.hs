{-# LANGUAGE NoImplicitPrelude #-}

import System.IO

main = do
  h <- openFile "input.txt" ReadMode
  c1 <- hLookAhead h
  hPutChar stdout c1
  c2 <- hGetChar h
  hPutChar stdout c2
  hPutChar stdout '\n'
  hClose h
