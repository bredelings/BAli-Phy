{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.CList
    ( CList
    , c_cons
    , c_nil
    , listToCList
    , mapFrom
    , increment_int
    , equals_int
    ) where

import Compiler.FFI.Runtime (RuntimeValue)
import Data.Bool

data CList a

-- The public constructors establish that every stored element is
-- self-contained, so an existing CList is itself a runtime value.
instance RuntimeValue (CList a)

-- These builtins have type variables but refer to unlifted types.

-- These unlifted types represent C++ objects and lists through Runtime::Exp values.
    
foreign import ecall "Pair:c_pair"
    c_cons_raw :: a -> CList a -> CList a

foreign import ecall "Pair:c_nil"
    c_nil_raw :: CList a

c_cons :: RuntimeValue a => a -> CList a -> CList a
c_cons = c_cons_raw

c_nil :: RuntimeValue a => CList a
c_nil = c_nil_raw

-- If we use "error" here, then we get an error defining error in Compiler.Base
listToCList :: RuntimeValue a => [a] -> CList a
listToCList (x:xs) = c_cons x (listToCList xs)
listToCList _ = c_nil

-- This is an attempt to avoid importing anything.
-- If we import something, that something might already reference Foreign.String.unpack_cpp_string
-- Then during simplification we crash because unpack_cpp_string is already in free_vars when we
--   try to define it.
foreign import ecall "Prelude:" increment_int :: Int -> Int

foreign import ecall "Prelude:" equals_int :: Int -> Int -> Bool

mapFrom :: Int -> Int -> (Int -> a) -> [a]
mapFrom j1 j2 f = go j1 where
    go i = case equals_int i j2 of
             True -> []
             _  -> f i : go (increment_int i)
