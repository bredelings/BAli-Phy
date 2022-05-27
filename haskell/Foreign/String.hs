{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Foreign.CList

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

map_from :: Int -> Int -> (Int -> a) -> [a]
map_from j1 j2 f = go j1 where
    go i = case subtract_int' i j2 of
             0# -> []
             _  -> f i : go (increment_int i)

unpack_cpp_string string = go string 0# where
    size = sizeOfString string
    go string i = case subtract_int' i size of
                    0# -> []
                    _ -> getStringElement string i : go string (increment_int i)

clist_to_string :: CList Char -> CPPString
builtin clist_to_string 1 "clist_to_string" "Vector"

list_to_string :: [Char] -> CPPString
list_to_string x  = clist_to_string (list_to_CList x)

