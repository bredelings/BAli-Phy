{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
module Main where

import Compiler.IO (IO(IO))
import Compiler.Prim (coerce)

newtype Age = Age Int

type family F a

data Nominal a = Nominal (F a)

bad :: Nominal Age -> Nominal Int
bad = coerce

main = IO (\s -> (s, ()))
