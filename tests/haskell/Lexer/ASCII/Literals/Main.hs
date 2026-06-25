{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO (putStrLn)
import Text.Show (show)

main = do
  putStrLn (show 'a')
  putStrLn (show '\n')
  putStrLn (show '\t')
  putStrLn (show '\r')
  putStrLn (show '\'')
  putStrLn (show '\\')
  putStrLn (show '"')
  putStrLn (show "ab\n\t\\\"'")
