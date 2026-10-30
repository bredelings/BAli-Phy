{-# LANGUAGE NoImplicitPrelude #-}
-- Neither absent export lists nor whole-module re-exports propagate defaults.
-- Failure here detects accidental export or transitive collection of the declaration.
module Main where
import Compiler.Error (error)
import System.IO (print)
import Local
import Whole
main = print (choose (error "unused"))
