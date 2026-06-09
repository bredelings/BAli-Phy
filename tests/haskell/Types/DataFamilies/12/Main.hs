{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (IO, putStrLn)

import DataFamilySupport

main :: IO ()
main = do
    FA () <- makeFA
    putStrLn message
