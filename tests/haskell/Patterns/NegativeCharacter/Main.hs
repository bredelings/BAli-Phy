{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Bool

f (-'b') = True
f _ = False
