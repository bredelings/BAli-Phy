{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ratio where

import Compiler.Num -- for Integer

data Ratio a = Ratio a a

type Rational = Ratio Integer

