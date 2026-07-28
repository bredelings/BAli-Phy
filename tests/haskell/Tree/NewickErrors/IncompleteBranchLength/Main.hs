{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base (String)
import System.IO (IO, putStrLn)
import Tree.Newick (parse_newick, print_newick)

newick :: String
newick = "(1:1e,2:0.1);"

main :: IO ()
main = putStrLn (print_newick (parse_newick newick))
