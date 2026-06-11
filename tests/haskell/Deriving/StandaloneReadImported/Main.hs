{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import System.IO (IO, putStrLn)
import Text.Read (Read, read)

import ReadSupport (Color(..))

deriving stock instance Read Color

main :: IO ()
main = case read "Red" of
    Red -> putStrLn "ok"
    Blue -> putStrLn "bad"
