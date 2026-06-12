{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box = forall a. Box { value :: a, tag :: Int }

main = print (value (Box 3 4))
