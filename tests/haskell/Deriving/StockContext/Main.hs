{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import Data.Maybe (Maybe, Maybe(Just))
import System.IO (print)

data NoEq = NoEq
data Phantom a = Phantom deriving Eq
data Applied f a b = Applied (f a) deriving Eq
data Chain a = Nil | Link a (Chain a) deriving Eq
data Even a = EvenNil | EvenValue (Odd a) deriving Eq
data Odd a = OddValue a deriving Eq

ok = (Phantom :: Phantom NoEq) == Phantom
  && (Applied (Just 3) :: Applied Maybe Int NoEq) == Applied (Just 3)
  && Link 1 (Link 2 Nil) == Link 1 (Link 2 Nil)
  && EvenValue (OddValue 4) == EvenValue (OddValue 4)

main = print (if ok then (1 :: Int) else 0)
