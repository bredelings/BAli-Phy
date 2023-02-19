{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base (String,
                      seq,
                      ($!),
                      error) where

import Compiler.Prim   -- for seq
import Compiler.Error  -- for error

type String = [Char]

infixr 0 $!
f $! x = x `seq` f x

