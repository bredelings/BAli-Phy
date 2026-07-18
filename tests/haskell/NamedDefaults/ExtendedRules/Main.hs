{-# LANGUAGE ExtendedDefaultRules, NoImplicitPrelude #-}
module Main where

import Compiler.Base (String)
import Compiler.Error (error)
import Compiler.Num
import Data.Eq (Eq)
import Data.Foldable (Foldable)
import Data.Function (($))
import System.IO (putStrLn)

choose :: (Eq a, Foldable f) => a -> f Int -> String
choose _ _ = "extended"

main = putStrLn $ choose (error "undefined") (error "undefined")
