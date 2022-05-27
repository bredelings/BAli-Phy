{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.CList where

data CList a

c_cons :: a -> CList a -> CList a
builtin c_cons 2 "c_pair" "Pair"

-- A builtin must have at least one argument -- why?
c_nil :: Int -> CList a
builtin c_nil 1 "c_nil" "Pair"

-- If we use "error" here, then we get an error defining error in Compiler.Base
list_to_CList :: [a] -> CList a
list_to_CList (x:xs) = c_cons x (list_to_CList xs)
list_to_CList _ = c_nil 0#

