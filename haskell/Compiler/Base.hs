{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where

import Compiler.Error  -- for error

type String = [Char]

infixr 0 $!, `seq`
f $! x = x `seq` f x

foreign import bpcall "Prelude:seq" seq :: a -> b -> b

foreign import bpcall "Prelude:struct_seq" struct_seq :: a -> b -> b
