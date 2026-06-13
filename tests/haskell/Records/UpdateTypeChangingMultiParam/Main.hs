{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import Data.Bool
import System.IO (print)

data Pair a b = Pair { first :: a, second :: b, tag :: Int }

changeBoth :: Pair Int Char -> Pair Bool Int
changeBoth p = p { first = True, second = 9 }

main = print (tag (changeBoth (Pair 3 'x' 11)))
