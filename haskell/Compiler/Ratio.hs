{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Ratio where

data Ratio a = Ratio a a

type Rational = Ratio Integer

numerator (Ratio x _) = x

denominator (Ratio _ y) = y

