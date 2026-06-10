{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Bool
import Data.Eq
import System.IO
import Text.Read

data Color = Red | Blue | Green deriving Eq deriving Read
data Box a = Box a deriving Eq deriving Read
data PairBox = PairBox Int Color Double deriving Eq deriving Read
data Fancy = Fancy (Int, Color) [Int] [Char] Char deriving Eq deriving Read
newtype Age = Age Int deriving Eq deriving Read

ok :: Bool
ok =
    read "Red" == Red &&
    read "  Box Blue" == Box Blue &&
    read "(Box Green)" == Box Green &&
    read "PairBox 3 Green 4.5" == PairBox 3 Green 4.5 &&
    read "Fancy (1,Blue) [2,3] \"hi\" 'x'" == Fancy (1, Blue) [2,3] "hi" 'x' &&
    read "Age 42" == Age 42 &&
    case (readsPrec 0 "GreenBlue" :: [(Color, [Char])]) of
        [] -> True
        _ -> False

main = putStrLn (if ok then "ok" else "bad")
