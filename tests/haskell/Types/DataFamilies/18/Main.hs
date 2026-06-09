{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (IO, putStrLn)

import DataFamilySupport (F(..), makeFA, message)

main :: IO ()
main = do
    FA () <- makeFA
    putStrLn message
