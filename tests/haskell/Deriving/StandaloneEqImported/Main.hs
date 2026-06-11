{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import Data.Eq (Eq((==)), not, (&&))
import System.IO (print)

import EqSupport (Color(..))

deriving stock instance Eq Color

main = print (if Red == Red && not (Red == Blue) then (1 :: Int) else 0)
