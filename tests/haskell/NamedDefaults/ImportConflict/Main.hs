{-# LANGUAGE NoImplicitPrelude #-}
-- Conflicting imports leave no default, including no restored Num fallback.
-- This distinguishes a genuine conflict from a silently selected import or built-in default.
module Main where
import Compiler.Num
import System.IO (print)
import A ()
import B ()
main = print 1
