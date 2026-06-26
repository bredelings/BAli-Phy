{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Foreign.String
import System.IO (print)

main = print (sizeOfString (cppSubString (list_to_string "abc") (-1) 1))
