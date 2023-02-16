{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base (String,
                      seq,
                      ($!),
                      error) where

import Compiler.Prim   -- for seq
import Compiler.Error  -- for error

type String = [Char]

infixr 0 $!, `seq`
f $! x = x `seq` f x

foreign import bpcall "Prelude:seq" seq :: a -> b -> b
