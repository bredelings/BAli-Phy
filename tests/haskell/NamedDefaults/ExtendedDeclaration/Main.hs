{-# LANGUAGE ExtendedDefaultRules, NoImplicitPrelude #-}
module Main where

import Compiler.Base (String)
import Compiler.Error (error)
import Compiler.Num
import Data.Eq (Eq)
import Data.Function (($))
import System.IO (putStrLn)

class Marker a where
    marker :: a -> String

instance Marker Int where
    marker _ = "Int"

default (Int)

select :: Eq a => a -> a
select x = x

main = putStrLn $ marker (select (error "undefined"))
