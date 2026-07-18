{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Main where

import System.IO (putStrLn)

class C a

data My = My
type Candidate = My

instance C My

default C (Candidate)

main = putStrLn "ok"
