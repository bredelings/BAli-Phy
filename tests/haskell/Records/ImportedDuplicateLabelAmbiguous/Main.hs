{-# LANGUAGE DuplicateRecordFields, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)
import RecordSupport (A(..), B(..))

setShared p = p { shared = 10 }

main = print (setShared (A 1 2))
