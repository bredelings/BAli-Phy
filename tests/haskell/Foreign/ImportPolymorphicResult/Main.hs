{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.FFI.Import

-- RawImport cannot select its terminal equation while 'a' might still become
-- a function type, even when a COutput dictionary is available.
invalidResult :: COutput a => RawImport a -> a
invalidResult = fromCOutput
