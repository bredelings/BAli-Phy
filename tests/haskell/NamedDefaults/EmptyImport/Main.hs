{-# LANGUAGE NoImplicitPrelude #-}
-- An imported empty declaration must suppress the built-in Num fallback;
-- ordinary successful-default tests cannot detect accidental fallback restoration.
module Main where
import Compiler.Num
import System.IO (print)
import A ()
main = print 1
