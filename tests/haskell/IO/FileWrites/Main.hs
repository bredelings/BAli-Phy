{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Base (String)
import Control.Monad (return)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
  ( IO, IOMode(ReadMode), appendFile, hClose, hGetContents', openFile, putStrLn,
    writeFile )

-- Check that the String and Text convenience functions write to their named files rather than stdout.
-- Direct Handle operations do not exercise the convenience functions' choice of destination.
main :: IO ()
main = do
  writeFile "obtained-string.txt" "one"
  appendFile "obtained-string.txt" " two"
  stringHandle <- openFile "obtained-string.txt" ReadMode
  stringContents <- hGetContents' stringHandle
  hClose stringHandle
  putStrLn stringContents

  TIO.writeFile "obtained-text.txt" (T.pack "three")
  TIO.appendFile "obtained-text.txt" (T.pack " four")
  textContents <- TIO.readFile "obtained-text.txt"
  TIO.putStrLn textContents
  return ()
