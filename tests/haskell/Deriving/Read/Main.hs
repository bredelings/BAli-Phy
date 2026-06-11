{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Bool
import Data.Eq
import Compiler.Num
import System.IO
import Text.Read

data Color = Red | Blue | Green deriving Eq deriving Read
data Box a = Box a deriving Eq deriving Read
data PairBox = PairBox Int Color Double deriving Eq deriving Read
data Fancy = Fancy (Int, Color) [Int] [Char] Char deriving Eq deriving Read
data Point = Point { pointX :: Int, pointColor :: Color } deriving Eq deriving Read
infixr 5 :+:
data Expr = Lit Int | Expr :+: Expr deriving Eq deriving Read
newtype Age = Age Int deriving Eq deriving Read

ok :: Bool
ok =
    read "Red" == Red &&
    read "  Box Blue" == Box Blue &&
    read "(Box Green)" == Box Green &&
    read "PairBox 3 Green 4.5" == PairBox 3 Green 4.5 &&
    read "Fancy (1,Blue) [2,3] \"hi\" 'x'" == Fancy (1, Blue) [2,3] "hi" 'x' &&
    read "Point {pointX = 7, pointColor = Blue}" == Point 7 Blue &&
    read "Lit 1 :+: (Lit 2 :+: Lit 3)" == (Lit 1 :+: (Lit 2 :+: Lit 3)) &&
    read "Age 42" == Age 42 &&
    read "  123456789012345678901234567890" == (123456789012345678901234567890 :: Integer) &&
    read "-123456789012345678901234567890" == (0 - 123456789012345678901234567890 :: Integer) &&
    (case (readsPrec 0 "123456789012345678901234567890xyz" :: [(Integer, [Char])]) of
        [(n, rest)] -> n == 123456789012345678901234567890 && rest == "xyz"
        _ -> False) &&
    (case (readsPrec 0 "+abc" :: [(Integer, [Char])]) of
        [] -> True
        _ -> False) &&
    (case readInfixConstructor ":+:" ":++ Lit 2" of
        [] -> True
        _ -> False) &&
    (case (readsPrec 0 "GreenBlue" :: [(Color, [Char])]) of
        [] -> True
        _ -> False)

main = putStrLn (if ok then "ok" else "bad")
