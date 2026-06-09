{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (IO, putStrLn)

type F :: (* -> *) -> *

data family F c

data instance F [] = FList

main :: IO ()
main = case FList of
    FList -> putStrLn "standalone kind signature data family ok"
