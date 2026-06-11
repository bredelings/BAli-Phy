{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

infixr 5 :+
data List = Nil | Int :+ List

x :+ xs `pick` y = x + y

main = print ((3 :+ Nil) `pick` 4)
