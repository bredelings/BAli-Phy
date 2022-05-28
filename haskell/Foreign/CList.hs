{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.CList where

data CList a

c_cons :: a -> CList a -> CList a
foreign import bpcall "Pair:c_pair" c_cons :: () -> () -> ()

-- A foreign import bpcall must have at least one argument -- why?
c_nil :: Int -> CList a
foreign import bpcall "Pair:c_nil" c_nil :: () -> ()

-- If we use "error" here, then we get an error defining error in Compiler.Base
list_to_CList :: [a] -> CList a
list_to_CList (x:xs) = c_cons x (list_to_CList xs)
list_to_CList _ = c_nil 0#

-- This is an attempt to avoid importing anything.
-- If we import something, that something might already reference Foreign.String.unpack_cpp_string
-- Then during simplification we crash because unpack_cpp_string is already in free_vars when we
--   try to define it.
increment_int :: Int -> Int
foreign import bpcall "Prelude:increment_int" increment_int :: () -> ()

subtract_int' :: Int -> Int -> Int
foreign import bpcall "Prelude:subtract_int" subtract_int' :: () -> () -> ()

map_from :: Int -> Int -> (Int -> a) -> [a]
map_from j1 j2 f = go j1 where
    go i = case subtract_int' i j2 of
             0# -> []
             _  -> f i : go (increment_int i)

