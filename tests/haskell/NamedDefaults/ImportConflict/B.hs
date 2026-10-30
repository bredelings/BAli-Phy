{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module B (default Num) where
import Compiler.Num (Num)
default Num (Double)
