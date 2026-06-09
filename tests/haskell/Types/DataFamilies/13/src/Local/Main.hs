{-# LANGUAGE NoImplicitPrelude #-}

module Local.Main where

import System.IO (IO, putStrLn)

import Local.DataFamilySupport

main :: IO ()
main = do
    FA () <- makeFA
    putStrLn message
