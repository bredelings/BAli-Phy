{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Qualified where

import qualified Compiler.Num as N

default N.Num (Int)
