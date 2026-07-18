{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Maybe

f (-(Just x)) = x
