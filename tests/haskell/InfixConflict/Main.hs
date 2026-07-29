{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num

infix 4 +++
infix 4 ***

x +++ _ = x
x *** _ = x

a = 1 +++ 2 *** 3
b = 1 *** 2 +++ 3
