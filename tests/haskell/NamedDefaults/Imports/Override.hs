{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Override (result) where
import Compiler.Error (error)
import Reexport
default Choose (Char)
result = (choose (error "unused"), other (error "unused"))
