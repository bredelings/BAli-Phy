{-# LANGUAGE NoImplicitPrelude, MultiWayIf #-}

import Compiler.Num
import Data.Bool
import System.IO (print)

main = print (if { | False -> (1 :: Int) | True -> 3 })
