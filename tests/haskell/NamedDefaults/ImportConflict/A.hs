{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module A (default Num) where
import Compiler.Num (Num)
default Num (Int)
