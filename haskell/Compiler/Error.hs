{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Error (error) where

import Foreign.String

foreign import bpcall "Prelude:error" builtin_error :: CPPString -> a

error :: [Char] -> a
error x = builtin_error (list_to_string x)

