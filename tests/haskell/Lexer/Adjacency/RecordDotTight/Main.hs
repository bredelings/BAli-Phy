{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int }

main = print ((Point 7).x)
