{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

data CPPString

getStringElement :: CPPString -> Int -> Char
builtin getStringElement 2 "getStringElement" "Vector"

sizeOfString :: CPPString -> Int
builtin sizeOfString 1 "sizeOfString" "Vector"

listFromString s = unpack_cpp_string s

-- This is an attempt to avoid importing anything.
-- If we import something, that something might already reference Foreign.String.unpack_cpp_string
-- Then during simplification we crash because unpack_cpp_string is already in free_vars when we
--   try to define it.
increment_int :: Int -> Int
builtin increment_int 1 "increment_int" "Prelude"

subtract_int' :: Int -> Int -> Int
builtin subtract_int' 2 "subtract_int" "Prelude"

unpack_cpp_string string = go string 0# where
    size = sizeOfString string
    go string i = case subtract_int' i size of
                    0# -> []
                    _ -> getStringElement string i : go string (increment_int i)

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

clist_to_string :: CList Char -> CPPString
builtin clist_to_string 1 "clist_to_string" "Vector"

list_to_string :: [Char] -> CPPString
list_to_string x  = clist_to_string (list_to_CList x)
