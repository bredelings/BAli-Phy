{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.CList where

import Data.Bool

data CList a

-- These builtins have type variables but refer to unlifted types.

-- In order for these unlifted types to represent C++ objects and lists we currently
--   rely on expression_ref.
    
foreign import ecall "Pair:c_pair" c_cons :: a -> CList a -> CList a

foreign import ecall "Pair:c_nil" c_nil :: CList a

-- If we use "error" here, then we get an error defining error in Compiler.Base
listToCList :: [a] -> CList a
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

