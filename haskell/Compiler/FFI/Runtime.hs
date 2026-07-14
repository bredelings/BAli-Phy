{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.FFI.Runtime (RuntimeValue) where

import Data.Bool (Bool)

-- | An evaluated value whose Runtime::Exp representation is self-contained.
-- Such a value can be copied without retaining a machine closure or its
-- environment, and can therefore be stored in detached foreign containers.
class RuntimeValue a

instance RuntimeValue ()
instance RuntimeValue Int
instance RuntimeValue Integer
instance RuntimeValue Double
instance RuntimeValue Char
instance RuntimeValue Bool
