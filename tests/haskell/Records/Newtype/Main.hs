{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

newtype UserId = UserId { userId :: Int }

replace u = u { userId = userId u + 1 }

main = print (userId (replace (UserId { userId = 41 })))
