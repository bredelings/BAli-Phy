{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Error (error)
import Compiler.IO (IO(IO))
import Data.Bool (not, (&&))
import Data.Eq

data Color = Red | Blue deriving stock Eq

deriving stock instance Eq a => Eq [a]

ok = ([Red,Blue] == [Red,Blue]) && not ([Red] == [Blue])

main = if ok then IO (\s -> (s, ())) else error "standalone Eq [] failed"
