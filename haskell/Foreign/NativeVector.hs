{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.NativeVector (NativeVector) where

import Compiler.FFI.Runtime (RuntimeValue)

-- Native numeric vectors select their Eigen representation through the
-- nominal element type.
type role NativeVector nominal
data NativeVector a

instance RuntimeValue (NativeVector a)
