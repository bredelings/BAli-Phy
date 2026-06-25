{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.IO (putStrLn)
import Text.Show (show)

main = do
  putStrLn (show '\65')
  putStrLn (show "\65")
  putStrLn (show "\65\&5")
  putStrLn (show "a\&b")
  putStrLn (show "\10\&5")
