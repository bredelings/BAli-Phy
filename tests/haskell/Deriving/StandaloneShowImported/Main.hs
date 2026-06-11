{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import System.IO (IO, putStrLn)
import Text.Show (Show, show)

import ShowSupport (Color(..))

deriving stock instance Show Color

main :: IO ()
main = putStrLn (show Red)
